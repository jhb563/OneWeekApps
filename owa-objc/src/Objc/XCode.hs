{-|
Module      : Objc.XCode
Description : Start file structures for an Objective C project (app delegate and initial
              view controller)
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Objc.XCode
  ( initialVCHeader
  , initialVCImplementation
  , appDelegateHeader
  , appDelegateImplementation 
  , mainFileM )
  where

import Model.OWAAppInfo
import Objc.AbSyn
import Objc.Utils

initialVCHeader :: OWAAppInfo -> ObjcFile
initialVCHeader appInfo = ObjcFile 
  [ topCommentSection "ViewController.h" appInfo
  , uiKitImportsSection 
  , interfaceSection ]
  where
    interfaceSection = InterfaceSection
      "ViewController"
      (Just "UIViewController")
      Nothing
      []
      []
      []

initialVCImplementation :: OWAAppInfo -> ObjcFile
initialVCImplementation appInfo = ObjcFile 
  [ topCommentSection "ViewController.m" appInfo
  , ImportsSection [FileImport "ViewController.h"] 
  , ImplementationSection "ViewController" Nothing [lifecycleSection] ]
  where
    lifecycleSection = MethodImplementationListSection 
      (Just "Lifecycle") 
      [viewDidLoadMethod, updateViewConstraintsMethod]
    viewDidLoadMethod = ObjcMethod False "viewDidLoad" (SimpleType "void") [] 
      [ ExpressionStatement $ MethodCall (Var "super") (LibMethod "viewDidLoad" []) []
      , ExpressionStatement $ MethodCall 
        (PropertyCall SelfExpr "view")
        (LibMethod "setNeedsUpdateConstraints" []) [] ]
    updateViewConstraintsMethod = ObjcMethod 
      False "updateViewConstraints" (SimpleType "void") [] 
      [ExpressionStatement $ MethodCall (Var "super") (LibMethod "updateViewConstraints" []) []]

appDelegateHeader :: OWAAppInfo -> ObjcFile
appDelegateHeader appInfo = ObjcFile 
  [ topCommentSection "AppDelegate.h" appInfo
  , uiKitImportsSection 
  , interfaceSection ]
  where
    interfaceSection = InterfaceSection 
      "AppDelegate" 
      (Just "UIResponder") 
      Nothing
      ["UIApplicationDelegate"]
      [windowProperty]
      []
    windowProperty = ObjcProperty (PointerType "UIWindow") ["strong", "nonatomic"] "window"

appDelegateImplementation :: OWAAppInfo -> ObjcFile
appDelegateImplementation appInfo = ObjcFile 
  [ topCommentSection "AppDelegate.m" appInfo
  , ImportsSection [FileImport "AppDelegate.h", FileImport "ViewController.h"] 
  , ImplementationSection "AppDelegate" Nothing [appDelegateImplSection] ]
  where
    appDelegateImplSection = MethodImplementationListSection Nothing
      ( didLaunchFunction :
        map delegateFunction 
          [ "WillResignActive"
          , "DidEnterBackground"
          , "WillEnterForeground"
          , "DidBecomeActive"
          , "WillTerminate" 
          ])
    delegateFunction name = ObjcMethod False "application" (SimpleType "void")
      [ParamDef name (PointerType "UIApplication") "application"] []

mainFileM :: OWAAppInfo -> ObjcFile
mainFileM appInfo = ObjcFile
  [ topCommentSection "main.m" appInfo 
  , ImportsSection [ModuleImport "UIKit", FileImport "AppDelegate.h"]
  , CMainMethodSection
  ]

didLaunchFunction :: ObjcMethod
didLaunchFunction = ObjcMethod False "app" (SimpleType "BOOL")
  [ ParamDef "lication" (PointerType "UIApplication") "application"
  , ParamDef "didFinishLaunchingWithOptions" (PointerType "NSDictionary") "launchOptions" ]
  [ windowInit
  , bgAssign
  , vcInit
  , rootVCAssignment
  , windowAssignment
  , ExpressionStatement windowMakeVisible
  , ReturnStatement (BoolLit True) ]
  where
    mainScreen = MethodCall
      (MethodCall (Var "UIScreen") (LibMethod "mainScreen" []) [])
      (LibMethod "bounds" [])
      []
    windowAlloc = MethodCall (Var "UIWindow") (LibMethod "alloc" []) []
    libInitWithFrame = LibMethod "initWith" ["Frame"]
    windowInit = AssignStatement
      (VarDecl (PointerType "UIWindow") "window")
      (MethodCall windowAlloc libInitWithFrame [mainScreen])
    bgAssign = AssignStatement
      (PropertyCall (Var "window") "backgroundColor")
      (MethodCall (Var "UIColor") (LibMethod "whiteColor" []) [])
    vcAlloc = MethodCall (Var "ViewController") (LibMethod "alloc" []) []
    vcInit = AssignStatement
      (VarDecl (PointerType "ViewController") "mainViewController")
      (MethodCall vcAlloc (LibMethod "init" []) [])
    windowVar = Var "window"
    selfWindowExpr = PropertyCall SelfExpr "window"
    rootVCAssignment = AssignStatement
      (PropertyCall windowVar "rootViewController")
      (Var "mainViewController")
    windowAssignment = AssignStatement selfWindowExpr windowVar
    windowMakeVisible = MethodCall selfWindowExpr (LibMethod "makeKeyAndVisible" []) []
