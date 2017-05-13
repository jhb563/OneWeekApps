{-|
Module      : Swift.XCode
Description : Start file structures for a Swift project (app delegate and initial
              view controller)
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Swift.XCode
  ( initialVC
  , appDelegate )
  where

import Model.OWAAppInfo
import Swift.AbSyn
import Swift.Utils

-- | Creates the file structure for the initial view controller in a Swift project
initialVC :: OWAAppInfo -> SwiftFile
initialVC appInfo = SwiftFile
  [ extensionCommentSection "ViewController.swift" appInfo
  , uiKitImportSection 
  , ClassSection "ViewController" ["UIViewController"] [methodSection] ]
  where
    methodSection = MethodImplementationListSection 
      Nothing
      [ viewDidLoadMethod, updateViewConstraintsMethod ]

viewDidLoadMethod :: SwiftMethod
viewDidLoadMethod = SwiftMethod
  { isInitializer = False
  , qualifiers = ["override"]
  , name = "viewDidLoad"
  , returnType = Nothing
  , params = []
  , methodBody = [superStatement, viewUpdateStatement] }
  where
    superStatement = ExpressionStatement $
      MethodCall (Just (Var "super")) (LibMethod "viewDidLoad" []) []
    viewUpdateStatement = ExpressionStatement $
      MethodCall (Just (Var "view")) (LibMethod "setNeedsUpdateConstraints" []) []

updateViewConstraintsMethod :: SwiftMethod
updateViewConstraintsMethod = SwiftMethod
  { isInitializer = False
  , qualifiers = ["override"]
  , name = "updateViewConstraints"
  , returnType = Nothing
  , params = []
  , methodBody = [superStatement] }
  where
    superStatement = ExpressionStatement $
      MethodCall (Just (Var "super")) (LibMethod "updateViewConstraints" []) []

-- | Creates the file structure for the app delegate in a Swift project
appDelegate :: OWAAppInfo -> SwiftFile
appDelegate appInfo = SwiftFile 
  [ extensionCommentSection "AppDelegate.swift" appInfo 
  , uiKitImportSection 
  , ClassSpecifierSection "UIApplicationMain"
  , ClassSection "AppDelegate" ["UIResponder", "UIApplicationDelegate"] [windowSection, methodSection] ]
  where
    windowStatement = VarDecl [] "window" (OptionalType (SimpleType "UIWindow")) Nothing
    windowSection = StatementListSection Nothing True [windowStatement]
    methodNames =
      [ "applicationWillResignActive"
      , "applicationDidEnterBackground"
      , "applicationWillEnterForeground"
      , "applicationDidBecomeActive"
      , "applicationWillTerminate" ]
    methodSection = MethodImplementationListSection
      Nothing
      (launchMethod : map appDelegateMethod methodNames)

appDelegateMethod :: String -> SwiftMethod
appDelegateMethod methodName = SwiftMethod
  { isInitializer = False
  , qualifiers = []
  , name = methodName
  , returnType = Nothing
  , params = [ParamDef (Just "application") "application" (SimpleType "UIApplication")]
  , methodBody = [] }

launchMethod :: SwiftMethod
launchMethod = SwiftMethod
  { isInitializer = False
  , qualifiers = []
  , name = "application"
  , returnType = Just (SimpleType "Bool")
  , params = 
    [ ParamDef
        { paramLabelName = Just "application"
        , paramTitle = "application"
        , paramType = SimpleType "UIApplication" } 
    , ParamDef
        { paramLabelName = Just "didFinishLaunchingWithOptions"
        , paramTitle = "launchOptions"
        , paramType = OptionalType (DictionaryType (SimpleType "NSObject") (SimpleType "AnyObject")) } ]
  , methodBody = 
      [ windowStmt
      , backgroundStmt
      , mainVCStmt
      , rootVCStmt
      , keyAndVisibleStmt
      , returnStmt ] }
  where
    optionWindow = OptionalExpr (Var "window")
    unwrappedWindow = ExplicitExpr (Var "window")
    mainScreenParam = PropertyCall
      (MethodCall (Just (Var "UIScreen")) (LibMethod "mainScreen" []) [])
      "bounds"
    windowStmt = AssignStatement
      (Var "window")
      (MethodCall (Just (Var "UIWindow")) (LibMethod "init" [Just "frame"]) [mainScreenParam])
    backgroundStmt = AssignStatement
      (PropertyCall optionWindow "backgroundColor")
      (MethodCall (Just (Var "UIColor")) (LibMethod "whiteColor" []) [])
    mainVCStmt = LetDecl "mainViewController"
      (MethodCall Nothing (LibMethod "ViewController" []) [])
    rootVCStmt = AssignStatement
      (PropertyCall unwrappedWindow "rootViewController")
      (Var "mainViewController")
    keyAndVisibleStmt = ExpressionStatement $
      MethodCall (Just unwrappedWindow) (LibMethod "makeKeyAndVisible" []) []
    returnStmt = ReturnStatement $ BoolLit True
