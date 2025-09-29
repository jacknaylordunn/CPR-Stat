//
//  ContentView.swift
//  CPR Stat
//
//  Created by Jack Naylor Dunn on 12/09/2025.
//

import SwiftUI
import WatchConnectivity
import Charts
import AVFoundation

// MARK: - App Storage Keys
struct AppStorageKeys {
    static let hasCompletedOnboarding = "hasCompletedOnboarding"
}

// MARK: - SHARED DATA MODELS (Synchronize with watchOS)

struct CompressionDataPoint: Codable, Identifiable {
    var id = UUID() // Used by SwiftUI for list identification
    let timestamp: Date
    var depth: Double
    var recoilComplete: Bool

    // Exclude 'id' from Codable auto-synthesis to prevent decoding errors,
    // as the watch doesn't send this property.
    private enum CodingKeys: String, CodingKey {
        case timestamp, depth, recoilComplete
    }
}

enum CPRSessionState: String, Codable, Equatable {
    case connecting, ready, countdown, active, summary
}

enum RecoilState: String, Codable {
    case good, bad, waiting
}

enum WatchPosition: String, Codable, CaseIterable, Identifiable {
    case leftWrist = "Left Wrist", rightWrist = "Right Wrist", onChest = "On Chest"
    var id: String { self.rawValue }
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

// MARK: - PHONE-SIDE DATA MODELS
struct CPRSession: Codable, Identifiable, Hashable {
    let id: UUID
    var studentName: String
    var trainerNotes: String = ""
    let date: Date
    let patientType: PatientType
    let duration: TimeInterval
    let dataPoints: [CompressionDataPoint]
    
    var analysis: SessionAnalysis { SessionAnalysis(session: self) }
    
    static func == (lhs: CPRSession, rhs: CPRSession) -> Bool { lhs.id == rhs.id }
    func hash(into hasher: inout Hasher) { hasher.combine(id) }
}

extension CPRSession {
    /// Applies a statistical filter to remove outlier data points, common during rescue breaths.
    var sanitizedDataPoints: [CompressionDataPoint] {
        let initialDataPoints = self.dataPoints
        
        // Use a statistical approach (IQR outlier removal) to filter data if we have enough points.
        guard initialDataPoints.count > 20 else {
            // Fallback to a simple range filter for very short or sparse sessions.
            return initialDataPoints.filter { (1.0...10.0).contains($0.depth) }
        }

        let depths = initialDataPoints.map(\.depth).sorted()
        
        // Calculate Interquartile Range (IQR)
        let q1Index = Int(Double(depths.count) * 0.25)
        let q3Index = Int(Double(depths.count) * 0.75)
        let q1 = depths[q1Index]
        let q3 = depths[q3Index]
        let iqr = q3 - q1
        
        // Define bounds for outlier removal. 1.5 is a standard multiplier.
        let lowerBound = q1 - 1.5 * iqr
        let upperBound = q3 + 1.5 * iqr
        
        // Also enforce a hard physiological limit (e.g., 1cm to 10cm)
        let finalLowerBound = max(lowerBound, 1.0)
        let finalUpperBound = min(upperBound, 10.0)
        
        // FIX: Add a guard to prevent creating an invalid range, which causes a crash.
        guard finalLowerBound <= finalUpperBound else {
            // If the statistical bounds are invalid (e.g., from highly skewed data),
            // fall back to the simple filter to prevent a crash.
            return initialDataPoints.filter { (1.0...10.0).contains($0.depth) }
        }
        
        return initialDataPoints.filter { (finalLowerBound...finalUpperBound).contains($0.depth) }
    }
}

struct SessionAnalysis {
    let session: CPRSession
    let totalCompressions: Int, averageRate: Double, averageDepth: Double
    let goodRecoilPercent: Double, compressionFraction: Double
    let rateScore: Double, depthScore: Double, recoilScore: Double, fractionScore: Double
    let compressionTime: TimeInterval
    let pauseTime: TimeInterval

    var overallScore: Int {
        let weightedScore = (rateScore * 0.3) + (depthScore * 0.4) + (recoilScore * 0.2) + (fractionScore * 0.1)
        return max(0, min(100, Int(weightedScore.rounded())))
    }
    
    var improvementTips: [String] {
        var tips = [String]()
        if depthScore < 80 {
            let lower = String(format: "%.1f", session.patientType.targetDepthRange.lowerBound)
            let upper = String(format: "%.1f", session.patientType.targetDepthRange.upperBound)
            tips.append("Focus on consistent compression depth. Aim for \(lower)-\(upper) cm.")
        }
        if rateScore < 80 { tips.append("Maintain a steady rate between 100-120 compressions per minute.") }
        if recoilScore < 90 { tips.append("Ensure you allow the chest to fully recoil after each compression.") }
        if fractionScore < 85 { tips.append("Minimize interruptions to improve your compression fraction.") }
        if tips.isEmpty && overallScore > 90 { tips.append("Excellent performance! Keep up the great work.")}
        return tips
    }

    init(session: CPRSession) {
        self.session = session
        
        // Use the sanitized data points with outliers removed for all calculations.
        let sanitizedDataPoints = session.sanitizedDataPoints
        self.totalCompressions = sanitizedDataPoints.count

        // --- NEW GRADUATED DEPTH SCORING ---
        let depths = sanitizedDataPoints.map(\.depth)
        self.averageDepth = depths.isEmpty ? 0 : depths.reduce(0, +) / Double(depths.count)
        let targetDepthRange = session.patientType.targetDepthRange
        let depthTolerance: Double = 1.0 // Allow score to gracefully decrease over 1.0cm outside target
        
        let depthQualities = depths.map { depth -> Double in
            if targetDepthRange.contains(depth) {
                return 1.0
            } else {
                let distance = min(abs(depth - targetDepthRange.lowerBound), abs(depth - targetDepthRange.upperBound))
                return max(0, 1.0 - (distance / depthTolerance))
            }
        }
        let totalDepthQuality = depthQualities.reduce(0, +)
        self.depthScore = totalCompressions > 0 ? (totalDepthQuality / Double(totalCompressions)) * 100 : 0
        
        // --- NEW GRADUATED RATE SCORING ---
        var instantaneousRates: [Double] = []
        var calculatedCompressionTime: TimeInterval = 0
        if sanitizedDataPoints.count > 1 {
            for i in 1..<sanitizedDataPoints.count {
                let timeDiff = sanitizedDataPoints[i].timestamp.timeIntervalSince(sanitizedDataPoints[i-1].timestamp)
                if (0.25...2.0).contains(timeDiff) {
                    instantaneousRates.append(60.0 / timeDiff)
                    calculatedCompressionTime += timeDiff
                }
            }
        }
        self.averageRate = instantaneousRates.isEmpty ? 0 : instantaneousRates.reduce(0, +) / Double(instantaneousRates.count)
        
        let targetRateRange: ClosedRange<Double> = 100...120
        let rateTolerance: Double = 20.0 // Allow score to gracefully decrease over 20cpm outside target
        
        let rateQualities = instantaneousRates.map { rate -> Double in
            if targetRateRange.contains(rate) {
                return 1.0
            } else {
                let distance = min(abs(rate - targetRateRange.lowerBound), abs(rate - targetRateRange.upperBound))
                return max(0, 1.0 - (distance / rateTolerance))
            }
        }
        let totalRateQuality = rateQualities.reduce(0, +)
        self.rateScore = !instantaneousRates.isEmpty ? (totalRateQuality / Double(instantaneousRates.count)) * 100 : 0

        self.compressionTime = calculatedCompressionTime
        self.pauseTime = max(0, session.duration - self.compressionTime)
        
        self.compressionFraction = session.duration > 0 ? (self.compressionTime / session.duration) * 100 : 0
        self.fractionScore = max(0, self.compressionFraction)

        let goodRecoils = sanitizedDataPoints.filter(\.recoilComplete).count
        self.goodRecoilPercent = totalCompressions > 0 ? (Double(goodRecoils) / Double(totalCompressions)) * 100 : 0
        self.recoilScore = self.goodRecoilPercent
    }
}

struct ActiveCPRData {
    var rate: Double = 0; var depth: Double = 0
    var recoil: RecoilState = .waiting
    var timer: Int = 120; var countdown: Int = 3
    var compressionsInCycle: Int = 0
}

// MARK: - SESSION DATA STORAGE
class SessionStore: ObservableObject {
    @Published var sessions: [CPRSession] = []
    private static let fileURL = try! FileManager.default.url(for: .documentDirectory, in: .userDomainMask, appropriateFor: nil, create: true).appendingPathComponent("cprSessions.json")
    init() { loadSessions() }
    func loadSessions() { guard let data = try? Data(contentsOf: Self.fileURL), let decoded = try? JSONDecoder().decode([CPRSession].self, from: data) else { return }; self.sessions = decoded.sorted { $0.date > $1.date } }
    func saveSessions() { if let data = try? JSONEncoder().encode(sessions.sorted { $0.date > $1.date }) { try? data.write(to: Self.fileURL, options: .atomic) } }
    func addSession(_ session: CPRSession) { sessions.insert(session, at: 0); saveSessions() }
    func updateSession(_ session: CPRSession) {
        if let index = sessions.firstIndex(where: { $0.id == session.id }) {
            sessions[index] = session
            saveSessions()
        }
    }
    func deleteSession(at offsets: IndexSet) { sessions.remove(atOffsets: offsets); saveSessions() }
}

// MARK: - WATCH CONNECTIVITY MANAGER
class WatchConnectivityManager: NSObject, WCSessionDelegate, ObservableObject {
    @Published var activeCPRData = ActiveCPRData()
    @Published var sessionState: CPRSessionState = .connecting
    @Published var isWatchAppOpen = false
    @Published var completedSession: CPRSession?
    
    @Published var hapticsEnabled: Bool = false {
        didSet { sendMessage([WCMessageKey.hapticsEnabled.rawValue: hapticsEnabled]) }
    }
    @Published var isWatchDataVisible: Bool = true {
        didSet { sendMessage([WCMessageKey.isWatchDataVisible.rawValue: isWatchDataVisible]) }
    }
    
    private var session: WCSession = .default
    
    override init() {
        super.init()
        if WCSession.isSupported() {
            session.delegate = self
            session.activate()
        }
    }
    
    // MARK: - WCSession Delegate Lifecycle
    
    func session(_ s: WCSession, activationDidCompleteWith st: WCSessionActivationState, error: Error?) {
        DispatchQueue.main.async {
            self.isWatchAppOpen = s.isReachable
            if !s.isReachable { self.sessionState = .connecting }
        }
    }
    
    func sessionReachabilityDidChange(_ s: WCSession) {
        DispatchQueue.main.async {
            self.isWatchAppOpen = s.isReachable
            // If we lose the watch and aren't on a summary screen, revert to connecting.
            if !s.isReachable && self.sessionState != .summary {
                self.sessionState = .connecting
            }
        }
    }
    
    func sessionDidBecomeInactive(_ s: WCSession) { /* No action needed for iOS */ }
    func sessionDidDeactivate(_ s: WCSession) { s.activate() }
    
    // MARK: - Message Handling
    
    func session(_ s: WCSession, didReceiveMessage msg: [String : Any]) {
        DispatchQueue.main.async {
            if let watchReady = msg[WCMessageKey.isWatchReady.rawValue] as? Bool, watchReady {
                self.sessionState = .ready
                self.isWatchAppOpen = true
                return
            }
            
            guard let stateStr = msg[WCMessageKey.sessionState.rawValue] as? String,
                  let watchState = CPRSessionState(rawValue: stateStr) else {
                self.updateLiveMetrics(from: msg)
                return
            }
            
            self.sessionState = watchState
            
            switch watchState {
            case .summary:
                if !self.processCompletedSession(from: msg) {
                    print("ERROR: Could not decode completed session data from the watch.")
                }
                
            case .active:
                // Play a sound only when transitioning from countdown to active.
                // Check if the previous state was not active to prevent repeated haptics.
                if self.activeCPRData.countdown > 0 && self.sessionState != .active {
                    AudioServicesPlaySystemSound(1057) // System beep sound
                    self.sendCommand(.playStartHaptic)
                }
                // Update metrics for the active state
                self.updateLiveMetrics(from: msg)
                
            case .countdown:
                self.updateLiveMetrics(from: msg)
                
            case .ready, .connecting:
                self.activeCPRData = ActiveCPRData()
            }
        }
    }
    
    /// Helper to parse live CPR data from a message dictionary.
    private func updateLiveMetrics(from msg: [String: Any]) {
        if let rate = msg[WCMessageKey.rate.rawValue] as? Double { self.activeCPRData.rate = rate }
        if let depth = msg[WCMessageKey.depth.rawValue] as? Double { self.activeCPRData.depth = depth }
        if let recoilStr = msg[WCMessageKey.recoil.rawValue] as? String, let recoil = RecoilState(rawValue: recoilStr) { self.activeCPRData.recoil = recoil }
        if let timer = msg[WCMessageKey.timer.rawValue] as? Int { self.activeCPRData.timer = timer }
        if let countdown = msg[WCMessageKey.countdown.rawValue] as? Int { self.activeCPRData.countdown = countdown }
        if let cycle = msg[WCMessageKey.compressionsInCycle.rawValue] as? Int { self.activeCPRData.compressionsInCycle = cycle }
    }
    
    /// Decodes the final session data payload from the watch. Returns true on success.
    @discardableResult
    private func processCompletedSession(from msg: [String: Any]) -> Bool {
        guard let data = msg[WCMessageKey.sessionData.rawValue] as? Data,
              let duration = msg[WCMessageKey.sessionDuration.rawValue] as? TimeInterval,
              let ptStr = msg[WCMessageKey.patientType.rawValue] as? String,
              let pt = PatientType(rawValue: ptStr) else {
            print("WCM Error: Message for summary state was missing required data keys.")
            return false
        }
        
        do {
            let points = try JSONDecoder().decode([CompressionDataPoint].self, from: data)
            self.completedSession = .init(id: UUID(), studentName: "Student", date: Date(), patientType: pt, duration: duration, dataPoints: points)
            return true
        } catch {
            print("WCM FATAL: Failed to decode [CompressionDataPoint] from watch data. Error: \(error)")
            return false
        }
    }
    
    // MARK: - Commands Sent to Watch
    
    private func sendMessage(_ message: [String: Any]) {
        guard session.isReachable else { return }
        session.sendMessage(message, replyHandler: nil) { error in
            print("Error sending message to watch: \(error.localizedDescription)")
        }
    }
    
    func sendCommand(_ command: WCCommand) {
        sendMessage([WCMessageKey.command.rawValue: command.rawValue])
    }
    
    /// The primary action to start a CPR session from the phone.
    func startSession() {
        // No more complex handshake. Just tell the watch to start.
        sendCommand(.start)
    }
    
    func sendWatchPosition(_ pos: WatchPosition) { sendMessage([WCMessageKey.watchPosition.rawValue: pos.rawValue]) }
    func sendPatientType(_ type: PatientType) { sendMessage([WCMessageKey.patientType.rawValue: type.rawValue]) }
}


// MARK: - MAIN APP VIEW
struct PhoneView: View {
    @StateObject private var sessionStore = SessionStore()
    @StateObject private var connectivity = WatchConnectivityManager()
    @State private var isPresentingNewSession = false
    @State private var isShowingOnboarding = !UserDefaults.standard.bool(forKey: AppStorageKeys.hasCompletedOnboarding)

    var body: some View {
        NavigationStack {
            ZStack(alignment: .bottomTrailing) {
                SessionListView()
                    .navigationTitle("CPR Sessions")

                Button(action: { isPresentingNewSession = true }) {
                    Image(systemName: "plus")
                        .font(.system(size: 24, weight: .semibold))
                        .foregroundColor(.white)
                        .frame(width: 60, height: 60)
                        .background(Color.accentColor)
                        .clipShape(Circle())
                        .shadow(radius: 5, x: 0, y: 3)
                }
                .padding()
            }
            .sheet(isPresented: $isPresentingNewSession) {
                NewSessionView()
            }
            .sheet(isPresented: $isShowingOnboarding) {
                OnboardingView(isShowingOnboarding: $isShowingOnboarding)
                    .interactiveDismissDisabled()
            }
            .environmentObject(sessionStore)
            .environmentObject(connectivity)
        }
    }
}

// MARK: - Session List & Detail Views
struct SessionListView: View {
    @EnvironmentObject var sessionStore: SessionStore
    @EnvironmentObject var connectivity: WatchConnectivityManager
    
    var body: some View {
        List {
            ForEach(sessionStore.sessions) { session in
                NavigationLink(value: session) {
                    SessionRowView(session: session)
                }
            }
            .onDelete(perform: sessionStore.deleteSession)
        }
        .navigationDestination(for: CPRSession.self) { session in
            // This custom binding ensures that the SessionDetailView always has a fresh
            // version of the session, even if the underlying array changes, preventing crashes.
            let sessionBinding = Binding<CPRSession>(
                get: {
                    // On get, find the session in the store by its stable ID.
                    // Fall back to the passed-in session value if it's somehow not in the store.
                    sessionStore.sessions.first { $0.id == session.id } ?? session
                },
                set: { updatedSession in
                    // On set, find the session's index by its ID and update it.
                    sessionStore.updateSession(updatedSession)
                }
            )
            SessionDetailView(session: sessionBinding)
        }
        .overlay {
            if sessionStore.sessions.isEmpty {
                ContentUnavailableView("No Sessions", systemImage: "list.bullet.clipboard", description: Text("Tap the + button to start a new session."))
            }
        }
    }
}

struct SessionRowView: View {
    let session: CPRSession
    var body: some View { HStack(spacing: 15) { ScoreCircleView(score: session.analysis.overallScore, size: 50); VStack(alignment: .leading) { Text(session.studentName).font(.headline); Text(session.date.formatted(date: .numeric, time: .shortened)) }; Spacer(); let sanitizedPoints = session.sanitizedDataPoints; if !sanitizedPoints.isEmpty { SparklineView(dataPoints: sanitizedPoints) } }.padding(.vertical, 8) }
}

// MARK: - New Session Flow Views
struct NewSessionView: View {
    @EnvironmentObject var connectivity: WatchConnectivityManager
    @Environment(\.dismiss) var dismiss
    @State private var studentName: String = ""
    @State private var watchPosition: WatchPosition = .onChest
    @State private var patientType: PatientType = .adult
    
    var body: some View {
        NavigationStack {
            ZStack {
                switch connectivity.sessionState {
                case .connecting:
                    ConnectingView()
                case .ready:
                    ReadyView(studentName: $studentName, watchPosition: $watchPosition, patientType: $patientType, startAction: { connectivity.startSession() })
                case .countdown:
                    CountdownView(countdown: connectivity.activeCPRData.countdown)
                case .active:
                    ActiveSessionView(patientType: patientType)
                case .summary:
                    summaryBody
                }
            }
            .transition(.opacity.animation(.easeInOut(duration: 0.3)))
            .interactiveDismissDisabled(connectivity.sessionState != .ready && connectivity.sessionState != .summary)
            .onChange(of: watchPosition) { _, n in connectivity.sendWatchPosition(n) }
            .onChange(of: patientType) { _, n in connectivity.sendPatientType(n) }
            .onAppear {
                UIApplication.shared.isIdleTimerDisabled = true
                connectivity.sendPatientType(patientType)
                connectivity.sendWatchPosition(watchPosition)
            }
            .onDisappear {
                UIApplication.shared.isIdleTimerDisabled = false
                // If the user dismisses the sheet before finishing, reset the watch.
                if connectivity.sessionState != .ready {
                    connectivity.sendCommand(.reset)
                }
            }
        }
    }
    
    @ViewBuilder private var summaryBody: some View {
        if let s = connectivity.completedSession {
            SummaryContainerView(session: .init(id: s.id, studentName: studentName.isEmpty ? "Unnamed" : studentName, date: s.date, patientType: s.patientType, duration: s.duration, dataPoints: s.dataPoints)) {
                connectivity.sendCommand(.reset)
                dismiss()
            }
        } else {
            // Error case: State is summary, but we have no session data.
            VStack(spacing: 20) {
                Image(systemName: "exclamationmark.triangle.fill")
                    .font(.system(size: 50))
                    .foregroundColor(.orange)
                Text("Data Processing Error")
                    .font(.title.bold())
                Text("Could not process the session data received from the watch.")
                    .foregroundColor(.secondary)
                    .multilineTextAlignment(.center)
                Button("Discard Session") {
                    connectivity.sendCommand(.reset)
                    dismiss()
                }
                .buttonStyle(.bordered)
                .controlSize(.large)
            }
            .padding()
        }
    }
}

struct ReadyView: View {
    @EnvironmentObject var connectivity: WatchConnectivityManager
    @Binding var studentName: String
    @Binding var watchPosition: WatchPosition
    @Binding var patientType: PatientType
    var startAction: () -> Void
    @State private var isStarting = false
    
    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(alignment: .leading, spacing: 24) {
                    
                    VStack(alignment: .leading, spacing: 12) {
                        Text("Session Details")
                            .font(.title2.bold())
                            .padding(.horizontal)
                        
                        TextField("Student Name (Optional)", text: $studentName)
                            .padding()
                            .background(Color(UIColor.secondarySystemGroupedBackground))
                            .cornerRadius(12)
                            .padding(.horizontal)
                    }
                    
                    VStack(alignment: .leading, spacing: 12) {
                        Text("Patient Type")
                            .font(.title2.bold())
                            .padding(.horizontal)
                        
                        PatientTypeSelector(selection: $patientType)
                            .padding(.horizontal)
                    }

                    VStack(alignment: .leading, spacing: 12) {
                        Text("Watch Settings")
                            .font(.title2.bold())
                            .padding(.horizontal)
                        
                        VStack(spacing: 0) {
                            OptionRowToggle(title: "Haptic Pacer", systemImage: "applewatch.radiowaves.left.and.right", isOn: $connectivity.hapticsEnabled)
                            Divider().padding(.leading, 60)
                            OptionRowToggle(title: "Show Data on Watch", systemImage: "eye", isOn: $connectivity.isWatchDataVisible)
                            Divider().padding(.leading, 60)
                            OptionRowPicker(selection: $watchPosition)
                        }
                        .background(Color(UIColor.secondarySystemGroupedBackground))
                        .clipShape(RoundedRectangle(cornerRadius: 12))
                        .padding(.horizontal)
                        
                        if watchPosition == .onChest {
                            Label("For best results, disable Wrist Detection in the watch's Settings app.", systemImage: "exclamationmark.triangle.fill")
                                .font(.caption)
                                .foregroundColor(.orange)
                                .padding(.horizontal)
                        }
                    }
                }
                .padding(.top)
            }
            .background(Color(UIColor.systemGroupedBackground))
            .safeAreaInset(edge: .bottom) {
                VStack(spacing: 8) {
                    Button(action: {
                        isStarting = true
                        startAction()
                    }) {
                        HStack {
                            Spacer()
                            if isStarting {
                                ProgressView()
                            } else {
                                Label("Start Session", systemImage: "play.fill")
                                    .fontWeight(.bold)
                            }
                            Spacer()
                        }
                    }
                    .buttonStyle(.borderedProminent)
                    .controlSize(.large)
                    .disabled(!connectivity.isWatchAppOpen || isStarting)
                    
                    if !connectivity.isWatchAppOpen {
                        Text("Watch app not reachable. Please open CPR Stat on your watch.")
                            .font(.caption)
                            .foregroundColor(.red)
                            .frame(maxWidth: .infinity)
                    }
                }
                .padding()
                .background(.thinMaterial)
            }
            .navigationTitle("New Session")
            .navigationBarTitleDisplayMode(.inline)
        }
    }
}

struct ActiveSessionSettingsView: View {
    @EnvironmentObject var connectivity: WatchConnectivityManager
    @Environment(\.dismiss) var dismiss
    
    var body: some View {
        NavigationStack {
            Form {
                Section("Live Feedback") {
                    Toggle("Haptic Pacer", systemImage: "applewatch.radiowaves.left.and.right", isOn: $connectivity.hapticsEnabled)
                    Toggle("Show Data on Watch", systemImage: "eye", isOn: $connectivity.isWatchDataVisible)
                }
                
                Section("Session Control") {
                    Button {
                        connectivity.sendCommand(.recalibrate)
                        dismiss()
                    } label: {
                        Label("Recalibrate Watch Sensor", systemImage: "arrow.counterclockwise.circle")
                    }
                }
            }
            .navigationTitle("Session Settings")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .cancellationAction) {
                    Button("Done") { dismiss() }
                }
            }
        }
    }
}

struct ActiveSessionView: View {
    @EnvironmentObject var connectivity: WatchConnectivityManager
    let patientType: PatientType
    @State private var showSettingsSheet = false
    
    var body: some View {
        let data = connectivity.activeCPRData
        return VStack {
            HStack {
                Label(String(format: "%01d:%02d", data.timer/60, data.timer%60), systemImage: "timer")
                Spacer()
                Label("\(data.compressionsInCycle)", systemImage: "heart.fill")
            }
            .font(.title2.weight(.semibold))
            .monospacedDigit()
            .padding([.horizontal, .top])
            
            Spacer()
            
            IntegratedFeedbackView(depth: data.depth, rate: data.rate, recoilState: data.recoil, patientType: patientType)
            
            Spacer()

            Button(role: .destructive, action: { connectivity.sendCommand(.stop) }) {
                Label("Stop Session", systemImage: "stop.fill").fontWeight(.bold).frame(maxWidth: .infinity)
            }
            .buttonStyle(.borderedProminent)
            .controlSize(.large)
            .padding()
        }
        .navigationTitle("CPR In Progress")
        .navigationBarTitleDisplayMode(.inline)
        .toolbar {
            ToolbarItem(placement: .primaryAction) {
                Button {
                    showSettingsSheet = true
                } label: {
                    Label("Settings", systemImage: "gearshape.fill")
                }
            }
        }
        .sheet(isPresented: $showSettingsSheet) {
            ActiveSessionSettingsView()
        }
    }
}

// MARK: - Summary & Graph Views
struct SummaryContainerView: View {
    @State var session: CPRSession
    let dismissAction: () -> Void
    @EnvironmentObject var sessionStore: SessionStore
    
    init(session: CPRSession, dismissAction: @escaping () -> Void) {
        self._session = State(initialValue: session)
        self.dismissAction = dismissAction
    }

    var body: some View {
        NavigationStack {
            SessionDetailView(session: $session, isNewSession: true)
                .navigationTitle("Session Summary")
                .navigationBarTitleDisplayMode(.inline)
                .toolbar {
                    ToolbarItem(placement: .cancellationAction) {
                        Button("Discard", role: .destructive, action: dismissAction)
                    }
                    ToolbarItem(placement: .confirmationAction) {
                        Button("Save") {
                            sessionStore.addSession(session)
                            dismissAction()
                        }
                    }
                }
        }
    }
}

struct SessionDetailView: View {
    @Binding var session: CPRSession
    var isNewSession: Bool
    @State private var isEditing = false
    @State private var pdfURL: IdentifiableURL?
    
    init(session: Binding<CPRSession>, isNewSession: Bool = false) {
        self._session = session
        self.isNewSession = isNewSession
        self._isEditing = State(initialValue: isNewSession)
    }

    var body: some View {
        TabView {
            SummaryMetricsView(session: $session, isEditing: isEditing)
                .tabItem { Label("Summary", systemImage: "list.bullet.clipboard") }
            
            if !session.dataPoints.isEmpty {
                CompressionGraphView(session: session)
                    .tabItem { Label("Graph", systemImage: "chart.xyaxis.line") }
            }
        }
        .navigationTitle(isEditing ? "Editing Summary" : (session.studentName.isEmpty ? "Session Summary" : session.studentName))
        .toolbar { toolbarContent }
        .sheet(item: $pdfURL) { ShareSheet(activityItems: [$0.url]) }
    }

    @ToolbarContentBuilder
    private var toolbarContent: some ToolbarContent {
        ToolbarItemGroup(placement: .primaryAction) {
            if isEditing {
                Button("Done") { isEditing.toggle() }
            } else {
                Button("Edit", systemImage: "pencil") { isEditing.toggle() }
            }
            Button("Export", systemImage: "square.and.arrow.up") {
                Task {
                    if let url = await generatePDF(for: session, studentName: session.studentName) {
                        self.pdfURL = IdentifiableURL(url: url)
                    }
                }
            }
        }
    }
}

struct SummaryMetricsView: View {
    @Binding var session: CPRSession
    var isEditing: Bool
    
    var body: some View {
        ScrollView {
            VStack(spacing: 20) {
                if isEditing {
                    TextField("Student Name", text: $session.studentName)
                        .font(.largeTitle)
                        .fontWeight(.bold)
                        .textFieldStyle(.roundedBorder)
                        .padding(.horizontal)
                }
                
                ScoreCircleView(score: session.analysis.overallScore, size: 150)
                    .padding(.vertical)
                
                if !session.analysis.improvementTips.isEmpty {
                    ImprovementTipsView(tips: session.analysis.improvementTips)
                }
                
                CompressionSummaryCard(analysis: session.analysis)
                RateSummaryCard(analysis: session.analysis)
                FractionSummaryCard(analysis: session.analysis)
                
                TrainerNotesCard(notes: $session.trainerNotes, isEditing: isEditing)
            }
            .padding()
        }
        .background(Color(UIColor.systemGroupedBackground))
    }
}

// MARK: - Rebuilt Graph Views
private struct CompressionGraphView: View {
    let session: CPRSession
    @State private var selectedTimestamp: Date?

    private var startTime: Date {
        session.sanitizedDataPoints.first?.timestamp ?? session.date
    }
    
    /// A helper struct to make calculated pause periods identifiable for use in ForEach loops.
    private struct PausePeriod: Identifiable {
        let id = UUID()
        let startOffset: TimeInterval
        let endOffset: TimeInterval
        var duration: TimeInterval { endOffset - startOffset }
    }
    
    /// A helper struct for plotting the waveform graph.
    private struct WaveformPoint: Identifiable {
        let id = UUID()
        let timeOffset: TimeInterval
        let depth: Double
    }

    /// Calculates periods of no compression longer than 2 seconds from the RAW data.
    private var pauses: [PausePeriod] {
        guard session.dataPoints.count > 1 else { return [] }
        var detectedPauses: [PausePeriod] = []
        
        for i in 1..<session.dataPoints.count {
            let previousPoint = session.dataPoints[i-1]
            let currentPoint = session.dataPoints[i]
            let timeDiff = currentPoint.timestamp.timeIntervalSince(previousPoint.timestamp)
            
            if timeDiff > 2.0 { // A pause is defined as > 2 seconds between compressions.
                detectedPauses.append(PausePeriod(
                    startOffset: previousPoint.timestamp.timeIntervalSince(startTime),
                    endOffset: currentPoint.timestamp.timeIntervalSince(startTime)
                ))
            }
        }
        return detectedPauses
    }
    
    /// Generates a series of points to draw a V-shaped waveform for each compression.
    private var waveformData: [WaveformPoint] {
        let sourceData = session.sanitizedDataPoints
        guard !sourceData.isEmpty else { return [] }

        var points = [WaveformPoint]()
        points.append(WaveformPoint(timeOffset: 0, depth: 0))

        // Create the waveform from the compression data
        for i in 0..<sourceData.count - 1 {
            let curr = sourceData[i]
            let next = sourceData[i+1]
            let interval = next.timestamp.timeIntervalSince(curr.timestamp)

            // Add the current compression's peak depth
            points.append(WaveformPoint(timeOffset: curr.timestamp.timeIntervalSince(startTime), depth: curr.depth))

            // If there's a long pause, draw the line back to 0 and hold it there
            if interval > 2.0 {
                points.append(WaveformPoint(timeOffset: curr.timestamp.timeIntervalSince(startTime) + 0.25, depth: 0))
                points.append(WaveformPoint(timeOffset: next.timestamp.timeIntervalSince(startTime) - 0.25, depth: 0))
            } else {
            // Otherwise, add a recoil point (depth 0) at the midpoint in time
                let midpointTime = curr.timestamp.addingTimeInterval(interval / 2)
                points.append(WaveformPoint(timeOffset: midpointTime.timeIntervalSince(startTime), depth: 0))
            }
        }
        
        // Add the very last compression point and a final recoil point
        if let last = sourceData.last {
            points.append(WaveformPoint(timeOffset: last.timestamp.timeIntervalSince(startTime), depth: last.depth))
            points.append(WaveformPoint(timeOffset: last.timestamp.timeIntervalSince(startTime) + 0.25, depth: 0))
        }

        return points
    }
    
    private var selectedPointInfo: (point: CompressionDataPoint, rate: Double)? {
        guard let selectedTimestamp = selectedTimestamp else { return nil }
        
        let dataPoints = session.sanitizedDataPoints
        guard let closestPoint = dataPoints.min(by: {
            abs($0.timestamp.timeIntervalSince(selectedTimestamp)) < abs($1.timestamp.timeIntervalSince(selectedTimestamp))
        }) else { return nil }
        
        // Don't show annotation if the scrub is too far from an actual data point
        guard abs(closestPoint.timestamp.timeIntervalSince(selectedTimestamp)) < 0.5 else { return nil }

        guard let index = dataPoints.firstIndex(where: { $0.id == closestPoint.id }) else {
            return (closestPoint, 0)
        }

        let rate: Double
        if index > 0 {
            let prevPoint = dataPoints[index - 1]
            let timeDiff = closestPoint.timestamp.timeIntervalSince(prevPoint.timestamp)
            rate = timeDiff > 0 ? (60.0 / timeDiff) : 0
        } else {
            rate = 0
        }
        
        return (closestPoint, rate)
    }
    
    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            if !session.dataPoints.isEmpty {
                chartHeader
                chartBody
            } else {
                ContentUnavailableView("No Graph Data",
                                       systemImage: "chart.xyaxis.line",
                                       description: Text("There is no compression data to display for this session."))
            }
        }
        .padding()
    }
    
    private var chartHeader: some View {
        VStack(alignment: .leading) {
            Text("Compression Timeline")
                .font(.headline)
            Text("Drag across the graph to inspect individual compressions.")
                .font(.caption)
                .foregroundColor(.secondary)
        }
        .padding(.bottom, 25)
    }

    private var chartBody: some View {
        Chart {
            // Draw the target depth zone as a green rectangle.
            RectangleMark(
                yStart: .value("Min Depth", session.patientType.targetDepthRange.lowerBound),
                yEnd: .value("Max Depth", session.patientType.targetDepthRange.upperBound)
            )
            .foregroundStyle(.green.opacity(0.2))

            // Draw shaded rectangles for each detected pause period.
            ForEach(pauses) { pause in
                RectangleMark(
                    xStart: .value("Pause Start", pause.startOffset),
                    xEnd: .value("Pause End", pause.endOffset)
                )
                .foregroundStyle(.orange.opacity(0.3))
                .annotation(position: .top, alignment: .leading) {
                    Text(String(format: "%.1fs pause", pause.duration))
                        .font(.caption2)
                        .bold()
                        .foregroundStyle(.orange)
                        .padding(.horizontal, 4)
                        .padding(.vertical, 2)
                        .background(.regularMaterial, in: .capsule)
                }
            }

            // Draw the compression depth as a line graph using the generated waveform data.
            ForEach(waveformData) { point in
                LineMark(
                    x: .value("Time", point.timeOffset),
                    y: .value("Depth", point.depth)
                )
                .foregroundStyle(Color.green)
                .interpolationMethod(.cardinal(tension: 0.5)) // Smoothed lines with controlled tension
            }
            
            // Show a rule mark and annotation when the user scrubs the chart.
            if let info = selectedPointInfo {
                RuleMark(x: .value("Selected", info.point.timestamp.timeIntervalSince(startTime)))
                    .lineStyle(StrokeStyle(lineWidth: 1.5, dash: [4, 4]))
                    .foregroundStyle(Color.secondary)
                    .annotation(position: .top, alignment: .center, spacing: 10) {
                        scrubberAnnotationView(info: info)
                    }
            }
        }
        .chartYScale(domain: [8.0, 0.0]) // Inverted Y-axis to represent depth, with 0cm at the top.
        .chartYAxis {
            AxisMarks(position: .leading, values: .automatic(desiredCount: 5)) { value in
                AxisGridLine()
                AxisTick()
                if let depth = value.as(Double.self) {
                     AxisValueLabel("\(Int(depth * 10)) mm")
                }
            }
        }
        .chartXAxis {
            AxisMarks(values: .stride(by: 30)) { value in
                AxisGridLine()
                AxisTick()
                if let interval = value.as(TimeInterval.self) {
                    let minutes = Int(interval) / 60
                    let seconds = Int(interval) % 60
                    AxisValueLabel(String(format: "%02d:%02d", minutes, seconds))
                }
            }
        }
        .chartOverlay { proxy in
            GeometryReader { geo in
                Rectangle().fill(.clear).contentShape(Rectangle())
                    .gesture(
                        DragGesture(minimumDistance: 0)
                            .onChanged { value in
                                guard let plotFrame = proxy.plotFrame else { return }
                                let xPos = value.location.x - geo[plotFrame].origin.x
                                if let timeOffset: TimeInterval = proxy.value(atX: xPos) {
                                    selectedTimestamp = startTime.addingTimeInterval(timeOffset)
                                }
                            }
                            .onEnded { _ in selectedTimestamp = nil }
                    )
            }
        }
    }
    
    private func scrubberAnnotationView(info: (point: CompressionDataPoint, rate: Double)) -> some View {
        VStack(alignment: .leading, spacing: 4) {
            Text(info.point.timestamp, format: .dateTime.hour().minute().second())
                .font(.caption.monospacedDigit())
                .foregroundColor(.secondary)
            
            Divider()
            
            HStack(spacing: 12) {
                VStack(alignment: .leading) {
                    Text("Depth").font(.caption2).foregroundStyle(.secondary)
                    Text("\(info.point.depth, specifier: "%.1f") cm").font(.headline).bold()
                }
                
                VStack(alignment: .leading) {
                    Text("Rate").font(.caption2).foregroundStyle(.secondary)
                    Text("\(info.rate, specifier: "%.0f") cpm").font(.headline).bold()
                }
            }
        }
        .padding(10)
        .background(.regularMaterial, in: .rect(cornerRadius: 8))
        .shadow(radius: 3)
    }
}


// MARK: - New Summary Components
struct ImprovementTipsView: View {
    let tips: [String]
    var body: some View {
        VStack(alignment: .leading, spacing: 5) {
            Label("Improvement Tips", systemImage: "lightbulb.fill").font(.headline)
            ForEach(tips, id: \.self) {
                Text("• \($0)").font(.subheadline)
            }
        }
        .padding()
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(Color(UIColor.secondarySystemGroupedBackground))
        .cornerRadius(12)
    }
}

struct TrainerNotesCard: View {
    @Binding var notes: String
    let isEditing: Bool

    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            Label("Trainer Notes", systemImage: "square.and.pencil").font(.title2.bold())
            
            if isEditing {
                TextEditor(text: $notes)
                    .frame(minHeight: 100)
                    .padding(4)
                    .background(Color(UIColor.systemGray6))
                    .cornerRadius(8)
                    .overlay(RoundedRectangle(cornerRadius: 8).stroke(Color.gray.opacity(0.2), lineWidth: 1))
            } else {
                Text(notes.isEmpty ? "No notes added." : notes)
                    .font(.body)
                    .foregroundColor(notes.isEmpty ? .secondary : .primary)
            }
        }
        .padding()
        .background(Color(UIColor.secondarySystemGroupedBackground))
        .cornerRadius(15)
    }
}

struct CompressionSummaryCard: View {
    let analysis: SessionAnalysis
    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            Label("Compressions", systemImage: "heart.fill").font(.title2.bold())
            HStack(alignment: .top) {
                VStack(alignment: .leading, spacing: 15) {
                    MetricRow(label: "Total Compressions", value: "\(analysis.totalCompressions)")
                    MetricRow(label: "Average Depth", value: String(format: "%.1f cm", analysis.averageDepth))
                    MetricRow(label: "Good Recoil", value: String(format: "%.0f%%", analysis.goodRecoilPercent))
                }
                Spacer()
                DepthSummaryGauge(analysis: analysis)
            }
        }
        .padding()
        .background(Color(UIColor.secondarySystemGroupedBackground))
        .cornerRadius(15)
    }
}

struct MetricRow: View {
    let label: String
    let value: String

    var body: some View {
        VStack(alignment: .leading) {
            Text(value)
                .font(.title2.weight(.semibold))
            Text(label)
                .font(.caption)
                .foregroundColor(.secondary)
        }
    }
}

struct DepthSummaryGauge: View {
    let analysis: SessionAnalysis
    private var depthPercent: Double { (analysis.depthScore) }
    
    var body: some View {
        VStack {
            ZStack {
                Circle()
                    .stroke(Color.blue.opacity(0.3), lineWidth: 10)
                Circle()
                    .trim(from: 0, to: CGFloat(depthPercent) / 100.0)
                    .stroke(Color.blue, style: StrokeStyle(lineWidth: 10, lineCap: .round))
                    .rotationEffect(.degrees(-90))
                Text("\(Int(depthPercent.rounded()))%")
                    .font(.title.bold())
            }
            .frame(width: 100, height: 100)
            
            Text("In Target Depth")
                .font(.caption)
        }
    }
}

struct RateSummaryCard: View {
    let analysis: SessionAnalysis
    private var ratePercent: Double { analysis.rateScore }
    
    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            Label("Compression Rate", systemImage: "metronome.fill").font(.title2.bold())
            HStack {
                MetricRow(label: "Average Rate", value: String(format: "%.0f cpm", analysis.averageRate))
                Spacer()
                VStack {
                    ZStack {
                        Circle()
                            .stroke(Color.green.opacity(0.3), lineWidth: 10)
                        Circle()
                            .trim(from: 0, to: CGFloat(ratePercent) / 100.0)
                            .stroke(Color.green, style: StrokeStyle(lineWidth: 10, lineCap: .round))
                            .rotationEffect(.degrees(-90))
                        Text("\(Int(ratePercent.rounded()))%")
                            .font(.title.bold())
                    }
                    .frame(width: 100, height: 100)
                    
                    Text("In Target Rate")
                        .font(.caption)
                }
            }
        }
        .padding()
        .background(Color(UIColor.secondarySystemGroupedBackground))
        .cornerRadius(15)
    }
}

struct FractionSummaryCard: View {
    let analysis: SessionAnalysis
    
    func formatInterval(_ interval: TimeInterval) -> String {
        let minutes = Int(interval) / 60
        let seconds = Int(interval) % 60
        return String(format: "%02d:%02d", minutes, seconds)
    }
    
    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            Label("Chest Compression Fraction", systemImage: "timer").font(.title2.bold())
            
            Text("\(Int(analysis.compressionFraction.rounded()))%")
                .font(.system(size: 40, weight: .bold))
            
            GeometryReader { geo in
                let totalWidth = geo.size.width
                let compressionRatio = analysis.session.duration > 0 ? analysis.compressionTime / analysis.session.duration : 0
                let compressionWidth = totalWidth * compressionRatio
                
                HStack(spacing: 0) {
                    Rectangle()
                        .fill(Color.accentColor)
                        .frame(width: compressionWidth)
                    Rectangle()
                        .fill(Color.orange.opacity(0.5))
                }
                .frame(height: 20)
                .clipShape(Capsule())
            }
            .frame(height: 20)
            
            HStack {
                VStack(alignment: .leading) {
                    LabelledText(label: "Compression Time", text: formatInterval(analysis.compressionTime), color: .accentColor)
                }
                Spacer()
                VStack(alignment: .trailing) {
                    LabelledText(label: "Pause Time", text: formatInterval(analysis.pauseTime), color: .orange)
                }
            }
        }
        .padding()
        .background(Color(UIColor.secondarySystemGroupedBackground))
        .cornerRadius(15)
    }
    
    private struct LabelledText: View {
        let label: String
        let text: String
        let color: Color
        
        var body: some View {
            HStack {
                Circle().fill(color).frame(width: 8, height: 8)
                Text(label).font(.caption)
            }
            Text(text)
                .font(.headline.monospacedDigit())
        }
    }
}

// MARK: - New Active Session Component
struct IntegratedFeedbackView: View {
    @Environment(\.verticalSizeClass) var verticalSizeClass
    
    let depth: Double
    let rate: Double
    let recoilState: RecoilState
    let patientType: PatientType

    private var isCompact: Bool { verticalSizeClass == .compact }

    // Computed properties for colors and recoil info
    private var depthColor: Color {
        guard depth > 0.1 else { return .gray }
        return patientType.targetDepthRange.contains(depth) ? .blue : .yellow
    }

    private var rateColor: Color {
        guard rate > 10 else { return .gray }
        return (100...120).contains(rate) ? .green : .yellow
    }

    private var recoilStateInfo: (c: Color, i: String, t: String) {
        switch recoilState {
        case .good: return (.green, "checkmark.circle.fill", "Good")
        case .bad: return (.orange, "arrow.down.circle.fill", "Release Fully")
        case .waiting: return (.gray, "pause.circle.fill", "Waiting")
        }
    }

    var body: some View {
        Group {
            if isCompact {
                // MARK: Wide Layout (Landscape & iPad)
                HStack(spacing: 10) {
                    depthGauge
                    rateIndicator
                    recoilIndicator
                }
            } else {
                // MARK: Compact Layout (Portrait iPhone)
                VStack(spacing: 16) {
                    depthGauge
                        .frame(maxHeight: 250)
                    
                    HStack(spacing: 16) {
                        rateIndicator
                        recoilIndicator
                    }
                }
            }
        }
        .padding()
        .animation(.spring(response: 0.3, dampingFraction: 0.6), value: depth)
        .animation(.spring(response: 0.3, dampingFraction: 0.6), value: rate)
    }

    private var depthGauge: some View {
        VStack(spacing: 8) {
            Text("Depth")
                .font(.headline)
                .foregroundColor(.secondary)

            ZStack(alignment: .top) {
                RoundedRectangle(cornerRadius: 30, style: .continuous)
                    .fill(Color(.systemGray6))

                GeometryReader { geo in
                    let h = geo.size.height
                    let displayMaxDepth: Double = 8.0
                    let targetRange = patientType.targetDepthRange
                    
                    let zoneTopY = (targetRange.lowerBound / displayMaxDepth) * h
                    let zoneBottomY = (targetRange.upperBound / displayMaxDepth) * h
                    let zoneHeight = max(0, zoneBottomY - zoneTopY)
                    
                    RoundedRectangle(cornerRadius: 25, style: .continuous)
                        .fill(Color.blue.opacity(0.2))
                        .frame(height: zoneHeight)
                        .offset(y: zoneTopY)
                        .padding(.horizontal, 5)

                    let depthRatio = depth / displayMaxDepth
                    let clampedRatio = max(0, min(1, depthRatio))
                    let indicatorHeight = isCompact ? 30.0 : 40.0
                    let indicatorY = (clampedRatio * (h - indicatorHeight))
                    
                    RoundedRectangle(cornerRadius: 15, style: .continuous)
                        .fill(depthColor.gradient)
                        .frame(height: indicatorHeight)
                        .overlay(
                             Capsule()
                                .fill(.white.opacity(0.3))
                                .frame(width: 40, height: 4)
                                .offset(y: 6)
                        )
                        .shadow(color: .black.opacity(0.2), radius: 3, y: 2)
                        .offset(y: indicatorY)
                }
            }
            .clipShape(RoundedRectangle(cornerRadius: 30, style: .continuous))

            Text(String(format: "%.1f cm", depth))
                .font(.system(size: isCompact ? 32 : 40, weight: .bold, design: .rounded))
                .foregroundColor(depthColor)
                .contentTransition(.numericText())
        }
    }
    
    private var rateIndicator: some View {
        VStack(spacing: isCompact ? 4 : 8) {
            Label("Rate", systemImage: "metronome.fill")
                .font(.headline)
                .foregroundColor(.secondary)
            
            Text(String(format: "%.0f", rate))
                .font(.system(size: isCompact ? 48 : 60, weight: .bold, design: .rounded))
                .foregroundColor(rateColor)
                .contentTransition(.numericText())
            
            Text("cpm")
                .font(.callout)
                .foregroundColor(.secondary)
        }
        .padding(isCompact ? 8 : 12)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .background(Color(UIColor.secondarySystemGroupedBackground))
        .cornerRadius(30, antialiased: true)
    }

    private var recoilIndicator: some View {
        VStack(spacing: isCompact ? 4 : 8) {
            let state = recoilStateInfo
            Label("Recoil", systemImage: "arrow.up.and.down.circle")
                .font(.headline)
                .foregroundColor(.secondary)
            
            Spacer()
            
            Image(systemName: state.i)
                .font(.system(size: isCompact ? 36 : 48, weight: .bold))
                .symbolRenderingMode(.palette)
                .foregroundStyle(.white, state.c)
                .frame(height: isCompact ? 48 : 60)

            Text(state.t)
                .font(.system(size: isCompact ? 18 : 24, weight: .bold, design: .rounded))
                .multilineTextAlignment(.center)
                .foregroundColor(state.c)
                .frame(minHeight: isCompact ? 24 : 30)
            
            Spacer()
        }
        .padding(isCompact ? 8 : 12)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .background(Color(UIColor.secondarySystemGroupedBackground))
        .cornerRadius(30, antialiased: true)
    }
}


// MARK: - Reusable UI Components
struct ConnectingView: View { var body: some View { VStack { Spacer(); ProgressView().scaleEffect(1.5); Text("Connecting to Apple Watch...").padding().font(.title3).foregroundColor(.secondary); Spacer() } } }

struct CountdownView: View {
    let countdown: Int
    var body: some View {
        VStack {
            Spacer()
            if countdown > 1 {
                Text("\(countdown)")
                    .font(.system(size: 120, weight: .bold, design: .rounded))
            } else {
                Text("Calibrating...")
                    .font(.largeTitle.bold())
                Text("Keep your watch steady on the chest.")
                    .font(.title3)
                    .foregroundColor(.secondary)
                    .padding(.top, 8)
            }
            Spacer()
        }
        .contentTransition(.numericText(countsDown: true))
    }
}

struct ScoreCircleView: View {
    let score: Int
    let size: CGFloat
    var color: Color { if score >= 90 { .green } else if score > 75 { .yellow } else { .orange } }
    var body: some View { ZStack { Circle().stroke(Color(.systemGray5), lineWidth: size * 0.1); Circle().trim(from: 0, to: CGFloat(score)/100.0).stroke(color, style: StrokeStyle(lineWidth: size * 0.1, lineCap: .round)).rotationEffect(.degrees(-90)); Text("\(score)").font(.system(size: size * 0.4, weight: .bold, design: .rounded)) }.frame(width: size, height: size).animation(.easeOut, value: score) }
}
struct MetricCard: View {
    let title: String, value: String, isGood: Bool
    var body: some View { VStack(alignment: .leading) { Text(title).font(.headline).foregroundColor(.secondary); Spacer(); Text(value).font(.largeTitle.bold().monospacedDigit()).foregroundColor(isGood ? .primary : .orange) }.padding().frame(minWidth: 160, minHeight: 100, alignment: .leading).background(Color(UIColor.secondarySystemGroupedBackground)).cornerRadius(15) }
}
struct SparklineView: View {
    let dataPoints: [CompressionDataPoint]
    var body: some View { if dataPoints.count > 1 { Chart(dataPoints) { LineMark(x: .value("T", $0.timestamp), y: .value("D", $0.depth)).interpolationMethod(.catmullRom) }.chartXAxis(.hidden).chartYAxis(.hidden).frame(width: 80, height: 30) } else { EmptyView() } }
}

// MARK: - PDF Exporting & Sharing
struct IdentifiableURL: Identifiable { let id = UUID(); let url: URL }
struct ShareSheet: UIViewControllerRepresentable {
    let activityItems: [Any]
    func makeUIViewController(context: Context) -> UIActivityViewController {
        UIActivityViewController(activityItems: activityItems, applicationActivities: nil)
    }
    func updateUIViewController(_ uiViewController: UIActivityViewController, context: Context) {}
}

@MainActor func generatePDF(for session: CPRSession, studentName: String) async -> URL? {
    var sessionToRender = session
    sessionToRender.studentName = studentName
    let viewToRender = SessionPDFView(session: sessionToRender)
    let renderer = ImageRenderer(content: viewToRender)
    let url = FileManager.default.temporaryDirectory.appendingPathComponent("\(session.id).pdf")
    renderer.render { size, context in
        var box = CGRect(x: 0, y: 0, width: 595, height: 842)
        guard let pdf = CGContext(url as CFURL, mediaBox: &box, nil) else { return }
        pdf.beginPDFPage(nil); context(pdf); pdf.endPDFPage(); pdf.closePDF()
    }
    return url
}
struct SessionPDFView: View {
    let session: CPRSession
    var body: some View {
        VStack(alignment: .leading, spacing: 20) {
            Text("CPR Stat Session Summary").font(.largeTitle.bold())
            Text("Generated on \(Date().formatted(date: .long, time: .shortened))")
            Divider()
            HStack(alignment: .top) {
                VStack(alignment: .leading) { Text("Student").font(.headline); Text(session.studentName) }
                Spacer()
                VStack(alignment: .leading) { Text("Date").font(.headline); Text(session.date.formatted(date: .numeric, time: .shortened)) }
                Spacer()
                VStack(alignment: .leading) { Text("Patient").font(.headline); Text(session.patientType.rawValue) }
            }
            Divider()
            ScoreCircleView(score: session.analysis.overallScore, size: 120)
            LazyVGrid(columns: [.init(.adaptive(minimum: 200))], spacing: 10) {
                MetricCard(title: "Avg. Rate", value: String(format: "%.0f cpm", session.analysis.averageRate), isGood: (100...120).contains(session.analysis.averageRate))
                MetricCard(title: "Avg. Depth", value: String(format: "%.1f cm", session.analysis.averageDepth), isGood: session.patientType.targetDepthRange.contains(session.analysis.averageDepth))
                MetricCard(title: "Good Recoil", value: String(format: "%.0f%%", session.analysis.goodRecoilPercent), isGood: session.analysis.goodRecoilPercent >= 90)
                MetricCard(title: "Compression Fraction", value: String(format: "%.0f%%", session.analysis.compressionFraction), isGood: session.analysis.compressionFraction >= 80)
            }
            if !session.trainerNotes.isEmpty {
                Divider()
                VStack(alignment: .leading) {
                    Text("Trainer Notes").font(.headline)
                    Text(session.trainerNotes)
                }
            }
        }
        .padding().frame(width: 595, height: 842).background(.white).foregroundColor(.black)
    }
}

// MARK: - Redesigned ReadyView Components

struct PatientTypeSelector: View {
    @Binding var selection: PatientType
    
    var body: some View {
        HStack(spacing: 10) {
            ForEach(PatientType.allCases) { type in
                Button(action: {
                    selection = type
                }) {
                    Text(type.rawValue)
                        .fontWeight(.semibold)
                        .frame(maxWidth: .infinity)
                        .padding()
                        .background(selection == type ? Color.accentColor : Color(UIColor.tertiarySystemGroupedBackground))
                        .foregroundColor(selection == type ? .white : .primary)
                        .cornerRadius(10)
                }
                .buttonStyle(.plain)
            }
        }
        .animation(.bouncy, value: selection)
    }
}

struct OptionRowToggle: View {
    let title: String
    let systemImage: String
    @Binding var isOn: Bool
    
    var body: some View {
        HStack(spacing: 15) {
            Image(systemName: systemImage)
                .font(.title2)
                .frame(width: 30)
                .foregroundColor(.accentColor)
            
            Toggle(title, isOn: $isOn)
        }
        .padding()
    }
}

struct OptionRowPicker: View {
    @Binding var selection: WatchPosition
    
    var body: some View {
        HStack(spacing: 15) {
            Image(systemName: "applewatch.watchface")
                .font(.title2)
                .frame(width: 30)
                .foregroundColor(.accentColor)
            
            Picker("Watch Position", selection: $selection) {
                ForEach(WatchPosition.allCases, id: \.self) {
                    Text($0.rawValue).tag($0)
                }
            }
        }
        .padding(.horizontal)
        .padding(.vertical, 5)
    }
}

// MARK: - Onboarding Views

struct OnboardingView: View {
    @Binding var isShowingOnboarding: Bool

    var body: some View {
        TabView {
            OnboardingPageView(
                imageName: "heart.text.square.fill",
                title: "Welcome to CPR Stat",
                description: "This app helps you measure and improve your CPR performance using your Apple Watch."
            )
            
            OnboardingPageView(
                imageName: "applewatch.watchface",
                title: "Watch Placement",
                description: "For the most accurate results, place your Apple Watch on the patient's chest. You can also use it on your wrist, but chest placement is recommended."
            )

            OnboardingPageView(
                imageName: "gearshape.fill",
                title: "Important Watch Setting",
                description: "To ensure continuous measurement when placed on the chest, please disable 'Wrist Detection' in your Apple Watch's Settings app under 'Passcode'."
            )

            OnboardingCompletionView(isShowingOnboarding: $isShowingOnboarding)
        }
        .tabViewStyle(.page(indexDisplayMode: .always))
        .background(Color(UIColor.systemGroupedBackground))
    }
}

struct OnboardingPageView: View {
    let imageName: String
    let title: String
    let description: String

    var body: some View {
        VStack(spacing: 30) {
            Image(systemName: imageName)
                .font(.system(size: 120, weight: .light))
                .foregroundColor(.accentColor)

            VStack(spacing: 15) {
                Text(title)
                    .font(.largeTitle.bold())
                Text(description)
                    .font(.title3)
                    .foregroundColor(.secondary)
                    .multilineTextAlignment(.center)
            }
        }
        .padding(30)
    }
}

struct OnboardingCompletionView: View {
    @Binding var isShowingOnboarding: Bool
    
    var body: some View {
        VStack(spacing: 30) {
            Image(systemName: "figure.cpr")
                .font(.system(size: 120, weight: .light))
                .foregroundColor(.accentColor)

            VStack(spacing: 15) {
                Text("You're All Set")
                    .font(.largeTitle.bold())
                Text("Tap the '+' button on the main screen to start your first CPR session.")
                    .font(.title3)
                    .foregroundColor(.secondary)
                    .multilineTextAlignment(.center)
            }
            
            Button(action: {
                // Set the flag in UserDefaults and dismiss the view
                UserDefaults.standard.set(true, forKey: AppStorageKeys.hasCompletedOnboarding)
                isShowingOnboarding = false
            }) {
                Text("Get Started")
                    .fontWeight(.bold)
                    .frame(maxWidth: .infinity)
            }
            .buttonStyle(.borderedProminent)
            .controlSize(.large)
            .padding(.top)
        }
        .padding(30)
    }
}


// MARK: - Helper Extensions
extension Color {
    var gradient: LinearGradient {
        LinearGradient(
            gradient: Gradient(colors: [self.opacity(0.8), self]),
            startPoint: .top,
            endPoint: .bottom
        )
    }
}

// MARK: - Previews
struct PhoneView_Previews: PreviewProvider { static var previews: some View { PhoneView().preferredColorScheme(.dark) } }
