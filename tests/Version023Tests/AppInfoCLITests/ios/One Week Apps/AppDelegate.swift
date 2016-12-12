//
// AppDelegate.swift
// One Week Apps
//
// Created By James Bowen 12/12/2016
// Copyright (c) 2016 One Week Apps. All Rights Reserved
//

import UIKit

@UIApplicationMain

class AppDelegate: UIResponder, UIApplicationDelegate {
  
  var window: UIWindow?
  
  func application(application: UIApplication, didFinishLaunchingWithOptions launchOptions: [NSObject: AnyObject]?) -> Bool {
    window = UIWindow.init(frame: UIScreen.mainScreen().bounds)
    window?.backgroundColor = UIColor.whiteColor()
    let mainViewController = ViewController()
    window!.rootViewController = mainViewController
    window!.makeKeyAndVisible()
    return true
  }
  
  func applicationWillResignActive(application: UIApplication) {
    
  }
  
  func applicationDidEnterBackground(application: UIApplication) {
    
  }
  
  func applicationWillEnterForeground(application: UIApplication) {
    
  }
  
  func applicationDidBecomeActive(application: UIApplication) {
    
  }
  
  func applicationWillTerminate(application: UIApplication) {
    
  }
  
}
