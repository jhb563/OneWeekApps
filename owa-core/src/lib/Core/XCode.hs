{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Core.XCode
Description : Generates code and files related to XCode
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Core.XCode (
  printBaseXCodeFiles  
) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Template
import           System.Directory

import           Core.XCode.Templates.Other
import           Core.XCode.Templates.ProjectFile
import           Model.OWAAppInfo
import           Swift.AbSyn
import           Swift.Print
import           Swift.Utils

---------------------------------------------------------------------------
------------------------ENTRY METHODS--------------------------------------
---------------------------------------------------------------------------

-- | Generates the initial files for an XCode project. The VC and AppDelegate
-- are written in Swift, by default.
printBaseXCodeFiles :: FilePath -> OWAAppInfo -> IO ()
printBaseXCodeFiles currentDirectory appInfo = do
  mapM_ 
    (createDirectoryIfMissing True) 
    (directoriesToCreate currentDirectory (appName appInfo))
  printVC currentDirectory appInfo
  printAppDelegate currentDirectory appInfo
  printInfo currentDirectory projName
  printContents currentDirectory projName 
  printPbxProj currentDirectory projName
  where
    projName = appName appInfo

---------------------------------------------------------------------------
------------------------FILE PRINTERS--------------------------------------
---------------------------------------------------------------------------

printVC :: FilePath -> OWAAppInfo -> IO ()
printVC dir info = printSwiftStructureToFile
  (initialVC info) 
  (vcPath dir (appName info)) 

printAppDelegate :: FilePath -> OWAAppInfo -> IO ()
printAppDelegate dir info = printSwiftStructureToFile
  (appDelegate info)
  (appDelegatePath dir (appName info))

printInfo :: FilePath -> String -> IO ()
printInfo dir projName = do
  let fullPath = infoPath dir projName
  writeFile fullPath infoPListTemplate

printPbxProj :: FilePath -> String -> IO ()
printPbxProj dir projName = writeFile fullPath (TL.unpack interpolatedText)
  where
    fullPath = pbxProjPath dir projName
    temp = template pbxProjTemplate
    context str = if str == "projectname" then T.pack projName else str
    interpolatedText = render temp context

printContents :: FilePath -> String -> IO ()
printContents dir projName = writeFile fullPath (TL.unpack interpolatedText)
  where
    fullPath = contentsPath dir projName
    temp = template contentsTemplate
    context str = if str == "projectname" then T.pack projName else str
    interpolatedText = render temp context

---------------------------------------------------------------------------
------------------------FILE BUILDERS--------------------------------------
---------------------------------------------------------------------------

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

appDelegate :: OWAAppInfo -> SwiftFile
appDelegate appInfo = SwiftFile 
  [ extensionCommentSection "AppDelegate.swift" appInfo 
  , uiKitImportSection 
  , ClassSpecifierSection "UIApplicationMain"
  , ClassSection "AppDelegate" ["UIResponder", "UIApplicationDelegate"] [windowSection, methodSection] ]
  where
    windowStatement = VarDecl [] "window" (OptionalType (SimpleType "UIWindow")) Nothing
    windowSection = StatementListSection Nothing [windowStatement]
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

---------------------------------------------------------------------------
------------------------DIRECTORIES AND FILES------------------------------
---------------------------------------------------------------------------

vcPath :: FilePath -> String -> FilePath
vcPath dir projName = baseProjectFilePath dir projName ++ "ViewController.swift"

appDelegatePath :: FilePath -> String -> FilePath
appDelegatePath dir projName = baseProjectFilePath dir projName ++ "AppDelegate.swift"

infoPath :: FilePath -> String -> FilePath
infoPath dir projName = baseProjectFilePath dir projName ++ "Info.plist"

pbxProjPath :: FilePath -> String -> FilePath
pbxProjPath dir projName = pbxProjDirPath dir projName ++ "project.pbxproj"

contentsPath :: FilePath -> String -> FilePath
contentsPath dir projName = pbxProjDirPath dir projName ++ ".xcworkspace/contents.xcworkspacedata"

baseProjectFilePath :: FilePath -> String -> FilePath
baseProjectFilePath dir projName = dir ++ "/ios/" ++ projName ++ "/"

pbxProjDirPath :: FilePath -> String -> FilePath
pbxProjDirPath dir projName = dir ++ "/ios/" ++ projName ++ ".xcodeproj/"

directoriesToCreate :: FilePath -> String -> [FilePath]
directoriesToCreate dir projName =
  [ dir ++ "/ios/" ++ projName
  , dir ++ "/ios/" ++ projName ++ ".xcodeproj/.xcworkspace" ]
