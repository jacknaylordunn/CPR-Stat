//
//  ContentView.swift
//  CPR Stat Watch App
//
//  Created by Jack Naylor Dunn on 12/09/2025.
//

import SwiftUI
import CoreMotion
import WatchConnectivity
import WatchKit

// MARK: - SHARED DATA MODELS (Synchronize with watchOS)

struct CompressionDataPoint: Codable {
    let timestamp: Date
    var depth: Double
    var recoilComplete: Bool
}

enum CPRSessionState: String, Codable, Equatable {
    // Shared states
    case ready, countdown, active, summary
}

enum RecoilState: String, Codable {
    case good, bad, waiting
}

enum WatchPosition: String, Codable, CaseIterable {
    case leftWrist = "Left Wrist"
    case rightWrist = "Right Wrist"
    case onChest = "On Chest"
}

enum PatientType: String, Codable, CaseIterable, Identifiable {
    case adult = "Adult", child = "Child", infant = "Infant"
    var id: String { self.rawValue }
    var targetDepthRange: ClosedRange<Double> {
        switch self {
        case .adult: return 5.0...6.0
        case .child: return 4.0...5.0
        case .infant: return 3.0...4.0
        }
    }
}

// MARK: - WATCH CONNECTIVITY DEFINITIONS

/// Provides type-safe keys for all WCSession messages, preventing typos.
enum WCMessageKey: String {
    case command
    case sessionState
    case isWatchReady
    
    case rate, depth, recoil
    case timer, countdown, compressionsInCycle
    
    case hapticsEnabled, isWatchDataVisible
    case watchPosition, patientType
    
    case sessionData, sessionDuration
}

/// Provides type-safe commands for controlling the watch.
enum WCCommand: String {
    case start, stop, reset
    case recalibrate, playStartHaptic
}


// MARK: - CPR ANALYSIS SERVICE
class CPRAnalysisService: NSObject, ObservableObject {
    
    private let motionManager = CMMotionManager()
    private let motionQueue = OperationQueue()
    private var session: WCSession = .default
    private var extendedSession: WKExtendedRuntimeSession?
    
    // MARK: Tunable Parameters for Physics-Based Model
    private let motionUpdateFrequency: Double = 100.0      // Hz, increased for better integration accuracy
    private let compressionStartThreshold: Double = 0.25   // g-force to begin registering a compression
    private let recoilCompleteThreshold: Double = 0.15     // g-force to consider recoil complete and stable
    private let depthUnitConversionFactor: Double = 100.0  // Converts meters from CoreMotion to cm
    private let maxDepthCutoff: Double = 12.0              // cm, a sanity check to prevent runaway integration
    private let maxRealisticAcceleration: Double = 4.0     // g-force, to reject non-CPR motion artifacts
    private let rateCalculationWindow = 10                 // Number of compressions to average for rate
    private let messageSendInterval: TimeInterval = 0.2    // Seconds (5Hz)
    
    // MARK: Published Properties for UI
    @Published var sessionState: CPRSessionState = .ready
    @Published var isResetting = false
    @Published var countdown: Int = 3
    @Published var timerSeconds: Int = 120
    @Published var currentRate: Double = 0.0
    @Published var currentDepth: Double = 0.0
    @Published var recoilState: RecoilState = .waiting
    @Published var compressionsInCurrentCycle: Int = 0
    @Published var isHapticPacingEnabled = false
    @Published var isDataVisible = true
    @Published var patientType: PatientType = .adult
    
    // MARK: Internal State
    private var watchPosition: WatchPosition = .onChest
    private var sessionData: [CompressionDataPoint] = []
    private var isCalibrating = false
    private var isResetRequested = false
    private var calibrationReadings: [Double] = []
    private var compressionAxisOffset = 0.0
    private var rateTimestamps: [Date] = []
    private var lastCompressionTimestamp: Date?
    private var lastMessageSendTime = Date()
    
    // MARK: Internal State for Physics Model
    private enum DetectionState { case waitingForDownward, inCompression, inRecoil }
    private var detectionState: DetectionState = .waitingForDownward
    private var velocity: Double = 0.0
    private var currentDisplacement: Double = 0.0
    private var lastMotionTimestamp: Date?
    
    // MARK: Timers
    private var sessionTimer: Timer?
    private var countdownTimer: Timer?
    private var hapticTimer: Timer?
    private var readyHeartbeatTimer: Timer?

    override init() {
        super.init()
        motionQueue.maxConcurrentOperationCount = 1
        motionQueue.name = "MotionAnalysisQueue"
        motionManager.deviceMotionUpdateInterval = 1.0 / motionUpdateFrequency
        if WCSession.isSupported() {
            session = .default
            session.delegate = self
            session.activate()
        }
    }
}

// MARK: - WCSession & Extended Runtime Session Delegate
extension CPRAnalysisService: WCSessionDelegate, WKExtendedRuntimeSessionDelegate {
    
    func session(_ s: WCSession, activationDidCompleteWith st: WCSessionActivationState, error: Error?) {
        if st == .activated {
            DispatchQueue.main.async {
                // On first launch or reactivation, ensure the app is in a clean state.
                if self.sessionState != .active && self.sessionState != .countdown {
                    self.performFullReset()
                }
            }
        }
    }
    
    func session(_ s: WCSession, didReceiveMessage msg: [String : Any]) {
        DispatchQueue.main.async {
            if let cmdStr = msg[WCMessageKey.command.rawValue] as? String, let cmd = WCCommand(rawValue: cmdStr) {
                self.handleCommand(cmd)
            }
            if let haptics = msg[WCMessageKey.hapticsEnabled.rawValue] as? Bool {
                self.isHapticPacingEnabled = haptics
                self.updateHapticTimerState()
            }
            if let posStr = msg[WCMessageKey.watchPosition.rawValue] as? String, let pos = WatchPosition(rawValue: posStr) {
                self.watchPosition = pos
            }
            if let vis = msg[WCMessageKey.isWatchDataVisible.rawValue] as? Bool {
                self.isDataVisible = vis
            }
            if let typeStr = msg[WCMessageKey.patientType.rawValue] as? String, let type = PatientType(rawValue: typeStr) {
                self.patientType = type
            }
        }
    }
    
    func extendedRuntimeSessionDidStart(_ e: WKExtendedRuntimeSession) {
        // Confirms the session is running. No action needed.
    }
    
    func extendedRuntimeSessionWillExpire(_ e: WKExtendedRuntimeSession) {
        // Proactively invalidate the session to trigger our centralized cleanup logic
        // instead of letting it error out.
        e.invalidate()
    }
    
    /// Central handler for when a runtime session ends for any reason.
    func extendedRuntimeSession(_ session: WKExtendedRuntimeSession, didInvalidateWith reason: WKExtendedRuntimeSessionInvalidationReason, error: Error?) {
        DispatchQueue.main.async {
            // Only handle invalidation for the session we currently know about.
            guard session === self.extendedSession else { return }

            // If a reset was requested, this is the highest priority.
            // It means we want to go back to the ready state, no matter what.
            if self.isResetRequested {
                self.isResetRequested = false // Consume the flag
                self.performFullReset()
                return
            }

            // If the session invalidated while active (e.g. timer expired, system interruption)
            // then we must go to the summary screen.
            if self.sessionState == .active {
                self.cleanupAfterActiveSession()
                self.sessionState = .summary
                if error != nil { WKInterfaceDevice.current().play(.failure) }
                self.sendSessionDataToPhone()
                
                // Start a new, short-lived session to keep the summary view on screen.
                // When the user dismisses the summary, this new session will be invalidated,
                // triggering a full reset via the logic below.
                self.startNewExtendedSession()
            } else {
                // If the session invalidated from any other state (e.g., countdown cancelled,
                // summary view timed out), just perform a full reset.
                self.performFullReset()
            }
        }
    }
}


// MARK: - Session Management
extension CPRAnalysisService {
    
    func startSession() {
        guard sessionState == .ready, !isResetting else { return }

        // Start an extended runtime session to keep the app alive during CPR
        extendedSession = WKExtendedRuntimeSession()
        extendedSession?.delegate = self
        extendedSession?.start()

        stopReadyHeartbeat()
        sessionState = .countdown
        countdown = 3
        startMotionUpdates()
        sendMessageToPhone()

        countdownTimer = .scheduledTimer(withTimeInterval: 1.0, repeats: true) { [weak self] _ in
            guard let self = self else { return }
            if self.countdown > 1 {
                self.countdown -= 1
                if self.countdown == 1 {
                    self.startCalibration()
                }
                self.sendMessageToPhone()
            } else {
                self.countdownTimer?.invalidate()
                self.countdownTimer = nil
                self.beginCPRAnalysis()
            }
        }
    }

    private func beginCPRAnalysis() {
        resetMetrics()
        sessionState = .active
        
        sessionTimer = .scheduledTimer(withTimeInterval: 1.0, repeats: true) { [weak self] _ in
            guard let self = self else { return }
            if self.timerSeconds > 0 {
                self.timerSeconds -= 1
            } else {
                self.endSession()
            }
        }

        DispatchQueue.main.async {
            self.sendMessageToPhone()
            self.updateHapticTimerState()
        }
    }

    /// Ends an active session or cancels a countdown.
    func endSession() {
        switch sessionState {
        case .countdown:
            countdownTimer?.invalidate(); countdownTimer = nil
            motionManager.stopDeviceMotionUpdates()
            // Invalidation triggers a full reset via the delegate.
            extendedSession?.invalidate()
            
        case .active:
            cleanupAfterActiveSession()
            sessionState = .summary
            WKInterfaceDevice.current().play(.success)
            sendSessionDataToPhone()
            // The session will be invalidated by the system when its time expires,
            // which will trigger the delegate to show the summary and start a new temp session.
            
        default:
            break
        }
    }

    /// Public method for UI and commands to request a reset back to the 'ready' state.
    func requestSessionReset() {
        guard !isResetting else { return }
        // Set a flag to ensure the invalidation delegate performs a full reset,
        // regardless of the state when the invalidation occurs.
        isResetRequested = true
        // Invalidating the session is the sole trigger for resetting the app state.
        // The `didInvalidateWith` delegate will handle the cleanup.
        extendedSession?.invalidate()
    }

    /// Centralized function to perform a full cleanup.
    private func performFullReset() {
        guard !isResetting else { return }
        isResetting = true
        isResetRequested = false // Ensure flag is cleared on any reset.
        
        cleanupAllTimersAndMotion()
        
        // The session that triggered this reset is now invalid.
        // We nil out our reference to it. We DO NOT call invalidate() here,
        // as this function is often called from the invalidation delegate itself,
        // which would cause a "session not running" error.
        extendedSession = nil
        
        isDataVisible = true
        isHapticPacingEnabled = false
        countdown = 3
        timerSeconds = 120
        resetMetrics()
        
        sessionState = .ready
        startReadyHeartbeat()
        
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
            self.isResetting = false
        }
    }
    
    private func startNewExtendedSession() {
        extendedSession = WKExtendedRuntimeSession()
        extendedSession?.delegate = self
        extendedSession?.start()
    }
    
    private func cleanupAllTimersAndMotion() {
        countdownTimer?.invalidate(); countdownTimer = nil
        sessionTimer?.invalidate(); sessionTimer = nil
        hapticTimer?.invalidate(); hapticTimer = nil
        stopReadyHeartbeat()
        motionManager.stopDeviceMotionUpdates()
    }
    
    private func cleanupAfterActiveSession() {
        sessionTimer?.invalidate(); sessionTimer = nil
        hapticTimer?.invalidate(); hapticTimer = nil
        motionManager.stopDeviceMotionUpdates()
    }
    
    private func updateHapticTimerState() {
        hapticTimer?.invalidate()
        hapticTimer = nil
        guard sessionState == .active, isHapticPacingEnabled else { return }
        let interval = 60.0 / 110.0 // 110 compressions per minute
        hapticTimer = .scheduledTimer(withTimeInterval: interval, repeats: true) { _ in
            WKInterfaceDevice.current().play(.directionUp)
        }
    }
}

// MARK: - Motion Processing
extension CPRAnalysisService {
    
    private func startCalibration() {
        isCalibrating = true
        calibrationReadings = []
        DispatchQueue.global(qos: .userInitiated).asyncAfter(deadline: .now() + 0.9) { [weak self] in
            self?.finishCalibration()
        }
    }
    
    private func finishCalibration() {
        isCalibrating = false
        guard !calibrationReadings.isEmpty else {
            compressionAxisOffset = 0.0
            return
        }
        let mean = calibrationReadings.reduce(0, +) / Double(calibrationReadings.count)
        compressionAxisOffset = mean
    }
    
    private func startMotionUpdates() {
        guard motionManager.isDeviceMotionAvailable else { return }
        
        self.lastMotionTimestamp = nil
        self.velocity = 0.0
        self.currentDisplacement = 0.0
        
        motionManager.startDeviceMotionUpdates(to: motionQueue) { [weak self] (motion, error) in
            guard let self = self, let motion = motion, error == nil else { return }
            
            let now = Date()
            let dt = self.lastMotionTimestamp.map { now.timeIntervalSince($0) } ?? 0
            self.lastMotionTimestamp = now
            
            guard dt > 0 && dt < 0.1 else { return }
            
            let rawForce = self.getRawForce(from: motion)
            
            if self.isCalibrating {
                self.calibrationReadings.append(rawForce)
                return
            }
            
            guard self.sessionState == .active else { return }
            
            let calibratedAcceleration = (rawForce - self.compressionAxisOffset)
            guard abs(calibratedAcceleration) < self.maxRealisticAcceleration else { return }

            self.processMotion(acceleration: calibratedAcceleration, dt: dt)
            
            if -self.lastMessageSendTime.timeIntervalSinceNow >= self.messageSendInterval {
                self.lastMessageSendTime = Date()
                DispatchQueue.main.async {
                    self.sendMessageToPhone()
                }
            }
        }
    }
    
    private func processMotion(acceleration: Double, dt: TimeInterval) {
        // --- Physics Integration ---
        velocity += acceleration * 9.8 * dt
        currentDisplacement += velocity * dt
        velocity *= 0.98 // Apply damping to reduce drift

        // Clamp displacement to a realistic range. Depth is positive downwards.
        currentDisplacement = max(-0.02, min(currentDisplacement, 0.12))
        
        let liveDepth = currentDisplacement * depthUnitConversionFactor
        DispatchQueue.main.async {
            // UI depth should not be negative.
            self.currentDepth = min(max(0, liveDepth), self.maxDepthCutoff)
        }

        // --- State Machine for Compression Detection ---
        switch detectionState {
        case .waitingForDownward:
            // We are at the top, waiting for a push. First, check if we are stable, which indicates good recoil.
            if liveDepth < 1.0 && abs(acceleration) < recoilCompleteThreshold {
                 processRecoil()
            }
            
            // A new compression starts with a clear downward acceleration.
            if acceleration > compressionStartThreshold {
                detectionState = .inCompression
                // A new push has started, so mark recoil state as bad until it completes.
                DispatchQueue.main.async { self.recoilState = .bad }
            }
            
        case .inCompression:
            // We are pushing down. The compression peak is reached when downward velocity stops.
            if velocity <= 0 {
                // Record the peak depth of the compression.
                if liveDepth > 1.0 { // Increased threshold to avoid noise
                    processCompression(depth: liveDepth)
                }
                // After the peak, we are in the recoil phase.
                detectionState = .inRecoil
            }
            
        case .inRecoil:
            // We are moving up, waiting to get back to the top of the chest.
            // We are considered back at the top when depth is minimal.
            if liveDepth < 1.0 {
                detectionState = .waitingForDownward
                // The logic in .waitingForDownward will confirm full recoil when the watch is stable.
            }
        }
    }
    
    private func getRawForce(from motion: CMDeviceMotion) -> Double {
        switch self.watchPosition {
        case .leftWrist, .rightWrist: return -motion.userAcceleration.y
        case .onChest: return -motion.userAcceleration.z
        }
    }
    
    private func processCompression(depth: Double) {
        let now = Date()
        if let lastTimestamp = lastCompressionTimestamp, now.timeIntervalSince(lastTimestamp) > 2.0 {
            DispatchQueue.main.async { self.compressionsInCurrentCycle = 0 }
        }
        lastCompressionTimestamp = now

        sessionData.append(CompressionDataPoint(timestamp: now, depth: depth, recoilComplete: false))
        
        rateTimestamps.append(now)
        if rateTimestamps.count > rateCalculationWindow { rateTimestamps.removeFirst() }
        var newRate = self.currentRate
        if rateTimestamps.count > 1, let first = rateTimestamps.first, let last = rateTimestamps.last, last.timeIntervalSince(first) > 0 {
            newRate = (Double(rateTimestamps.count - 1) / last.timeIntervalSince(first)) * 60.0
        }
                
        DispatchQueue.main.async {
            self.currentRate = newRate
            self.compressionsInCurrentCycle += 1
        }
    }

    private func processRecoil() {
        if let lastIndex = sessionData.indices.last {
            // Only mark recoil as complete on the most recent data point if it's not already marked.
            if !sessionData[lastIndex].recoilComplete {
                sessionData[lastIndex].recoilComplete = true
            }
        }
        DispatchQueue.main.async {
            // Only update the UI if the state is changing to avoid unnecessary updates.
            if self.recoilState != .good {
                self.recoilState = .good
            }
        }
    }
}

// MARK: - Communication
extension CPRAnalysisService {
    
    private func handleCommand(_ cmd: WCCommand) {
        switch cmd {
        case .start: if sessionState == .ready { startSession() }
        case .stop: endSession()
        case .reset: requestSessionReset()
        case .recalibrate: if sessionState == .active { startCalibration() }
        case .playStartHaptic: WKInterfaceDevice.current().play(.start)
        }
    }
    
    private func startReadyHeartbeat() {
        DispatchQueue.main.async {
            self.readyHeartbeatTimer?.invalidate()
            self.readyHeartbeatTimer = .scheduledTimer(withTimeInterval: 2.0, repeats: true) { [weak self] _ in
                self?.sendReadyStateToPhone()
            }
            self.sendReadyStateToPhone()
        }
    }
    
    private func stopReadyHeartbeat() {
        DispatchQueue.main.async {
            self.readyHeartbeatTimer?.invalidate()
            self.readyHeartbeatTimer = nil
        }
    }
    
    private func sendSessionDataToPhone() {
        let duration = 120.0 - Double(timerSeconds)
        guard let data = try? JSONEncoder().encode(sessionData) else { return }
        
        let msg: [String: Any] = [
            WCMessageKey.sessionState.rawValue: CPRSessionState.summary.rawValue,
            WCMessageKey.sessionData.rawValue: data,
            WCMessageKey.sessionDuration.rawValue: duration,
            WCMessageKey.patientType.rawValue: patientType.rawValue
        ]
        
        if session.isReachable { session.sendMessage(msg, replyHandler: nil) }
    }
    
    private func sendReadyStateToPhone() {
        if session.isReachable {
            let msg: [String: Any] = [WCMessageKey.isWatchReady.rawValue: true, WCMessageKey.sessionState.rawValue: CPRSessionState.ready.rawValue]
            session.sendMessage(msg, replyHandler: nil)
        }
    }
    
    private func sendMessageToPhone() {
        guard session.isReachable else { return }
        session.sendMessage(createMessagePayload(), replyHandler: nil)
    }
    
    private func createMessagePayload() -> [String: Any] {
        return [
            WCMessageKey.sessionState.rawValue: sessionState.rawValue,
            WCMessageKey.rate.rawValue: currentRate,
            WCMessageKey.depth.rawValue: currentDepth,
            WCMessageKey.recoil.rawValue: recoilState.rawValue,
            WCMessageKey.timer.rawValue: timerSeconds,
            WCMessageKey.countdown.rawValue: countdown,
            WCMessageKey.compressionsInCycle.rawValue: compressionsInCurrentCycle
        ]
    }
    
    private func resetMetrics() {
        currentRate = 0.0
        currentDepth = 0.0
        recoilState = .waiting
        compressionsInCurrentCycle = 0
        
        sessionData.removeAll()
        rateTimestamps.removeAll()
        
        lastCompressionTimestamp = nil
        velocity = 0.0
        currentDisplacement = 0.0
        lastMotionTimestamp = nil
        detectionState = .waitingForDownward
    }
}

// MARK: - MAIN WATCH SWIFTUI VIEW & COMPONENTS
struct WatchView: View {
    @StateObject private var cprService = CPRAnalysisService()
    var body: some View {
        VStack {
            switch cprService.sessionState {
            case .ready:
                ZStack {
                    Button(action: cprService.startSession) {
                        Text("START")
                            .font(.headline)
                            .fontWeight(.bold)
                            .frame(maxWidth: .infinity, minHeight: 60)
                            .background(cprService.isResetting ? Color.gray : Color.accentColor)
                            .cornerRadius(15)
                    }
                    .buttonStyle(.plain)
                    .disabled(cprService.isResetting)

                    if cprService.isResetting {
                        ProgressView()
                    }
                }
            case .countdown:
                CountdownWatchView(countdown: cprService.countdown)
            case .active:
                ActiveWatchView(cprService: cprService)
            case .summary:
                VStack(spacing: 15) {
                    Text("Session End").font(.headline)
                    Text("Check phone for results.").font(.caption).foregroundColor(.secondary)
                    Button("Done") { cprService.requestSessionReset() }
                }
            }
        }
        .padding()
        .animation(.default, value: cprService.sessionState)
    }
}

struct CountdownWatchView: View {
    let countdown: Int
    var body: some View {
        VStack {
            if countdown > 1 {
                Text("\(countdown)")
                    .font(.system(size: 80, weight: .bold))
            } else {
                Text("Calibrating...")
                    .font(.title2).bold()
                Text("Keep watch steady.")
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
        }
        .id(countdown)
        .transition(.scale.animation(.easeInOut))
    }
}

struct ActiveWatchView: View {
    @ObservedObject var cprService: CPRAnalysisService
    
    var body: some View {
        VStack {
            HStack {
                Text(String(format: "%01d:%02d", cprService.timerSeconds / 60, cprService.timerSeconds % 60))
                    .font(.title3)
                    .fontWeight(.semibold)
                    .foregroundColor(.accentColor)
                    .monospacedDigit()
                Spacer()
                Text("\(cprService.compressionsInCurrentCycle)")
                    .font(.title3)
                    .fontWeight(.semibold)
                    .foregroundColor(.secondary)
                    .monospacedDigit()
            }
            
            if cprService.isDataVisible {
                CompressionGuidanceView(
                    currentDepth: cprService.currentDepth,
                    currentRate: cprService.currentRate,
                    recoilState: cprService.recoilState,
                    targetDepthRange: cprService.patientType.targetDepthRange
                )
            } else {
                Spacer()
                VStack(spacing: 5) {
                    Image(systemName: "eye.slash.fill").font(.largeTitle)
                    Text("DATA HIDDEN").font(.caption).bold()
                }
                .foregroundColor(.secondary)
                Spacer()
            }
            
            Button(action: cprService.endSession) { Image(systemName: "stop.fill") }
                .tint(.red)
                .font(.title2)
                .padding(.top, 5)
        }
    }
}

struct CompressionGuidanceView: View {
    let currentDepth: Double, currentRate: Double, recoilState: RecoilState, targetDepthRange: ClosedRange<Double>
    private let maxDisplayableDepth: Double = 8.0
    private var depthPercentage: CGFloat { min(CGFloat(currentDepth / maxDisplayableDepth), 1.0) }
    private var rateColor: Color { (100.0...120.0).contains(currentRate) ? .green : .yellow }
    private var depthColor: Color { targetDepthRange.contains(currentDepth) ? .green : .yellow }
    private var recoilColor: Color { switch recoilState { case .good: .green; case .bad: .red; case .waiting: .gray } }

    var body: some View {
        HStack(spacing: 15) {
            GeometryReader { geometry in
                ZStack(alignment: .top) {
                    Capsule().fill(Color.secondary.opacity(0.3))
                    
                    let height = geometry.size.height
                    let zoneHeight = height * (targetDepthRange.upperBound - targetDepthRange.lowerBound) / maxDisplayableDepth
                    let zoneTopOffset = height * targetDepthRange.lowerBound / maxDisplayableDepth
                    
                    Capsule()
                        .fill(Color.green.opacity(0.4))
                        .frame(height: zoneHeight)
                        .offset(y: zoneTopOffset)
                    
                    Capsule()
                        .fill(depthColor)
                        .frame(height: depthPercentage * height)
                        .animation(.spring(), value: depthPercentage)
                }
            }
            .frame(width: 40)
            .clipShape(Capsule())
            
            VStack(alignment: .leading, spacing: 10) {
                MetricRow(label: "Rate", value: String(format: "%.0f", currentRate), color: rateColor, systemImage: "speedometer")
                MetricRow(label: "Depth", value: String(format: "%.1f", currentDepth), color: depthColor, systemImage: "arrow.down.to.line.compact")
                MetricRow(label: "Recoil", value: recoilState.rawValue.capitalized, color: recoilColor, systemImage: "arrow.up.and.down.and.arrow.left.and.right")
            }
            .font(.system(.caption, design: .rounded).weight(.semibold))
        }
    }
}

struct MetricRow: View {
    let label: String, value: String, color: Color, systemImage: String
    var body: some View { HStack { Image(systemName: systemImage).font(.body).foregroundColor(color).frame(width: 20); VStack(alignment: .leading) { Text(label.uppercased()).font(.caption2).foregroundColor(.secondary); Text(value) } } }
}

struct WatchView_Previews: PreviewProvider { static var previews: some View { WatchView() } }
