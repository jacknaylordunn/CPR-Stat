//
//  CPR_StatApp.swift
//  CPR Stat Watch App
//
//  Created by Jack Naylor Dunn on 12/09/2025.
//

import SwiftUI

@main
struct CPR_Stat_Watch_App: App {
    var body: some Scene {
        WindowGroup {
            // This now correctly points to the WatchView we created,
            // instead of the default 'ContentView'.
            WatchView()
        }
    }
}
