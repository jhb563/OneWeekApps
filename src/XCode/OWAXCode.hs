{-|
Module      : OWAXCode
Description : Generates code and files related to XCode
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAXCode (
  printBaseXCodeFiles  
) where

import OtherTemplates
import OWAAppInfo
import OWASwiftAbSyn
import OWASwiftPrint
import OWASwiftUtil
import System.Directory

---------------------------------------------------------------------------
------------------------ENTRY METHODS--------------------------------------
---------------------------------------------------------------------------

-- | Generates the initial files for an XCode project. The VC and AppDelegate
-- are written in Swift, by default.
printBaseXCodeFiles :: FilePath -> OWAAppInfo -> IO ()
printBaseXCodeFiles currentDirectory appInfo = do
  printVC currentDirectory appInfo
  printAppDelegate currentDirectory appInfo
  printInfo currentDirectory name
  printPbxProj currentDirectory name
  printContents currentDirectory name 
  where
    name = appName appInfo

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
printInfo dir name = do
  let fullPath = infoPath dir name
  writeFile fullPath infoPListTemplate

printPbxProj :: FilePath -> String -> IO ()
printPbxProj _ _ = return ()

printContents :: FilePath -> String -> IO ()
printContents _ _ = return ()

---------------------------------------------------------------------------
------------------------FILE BUILDERS--------------------------------------
---------------------------------------------------------------------------

initialVC :: OWAAppInfo -> SwiftFile
initialVC appInfo = SwiftFile
  [ extensionCommentSection "ViewController.swift" appInfo
  , uiKitImportSection 
  , ClassSection "ViewController" "UIViewController" [methodSection] ]
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
  [ extensionCommentSection "AppDelegate.swift" appInfo ]

---------------------------------------------------------------------------
------------------------DIRECTORIES AND FILES------------------------------
---------------------------------------------------------------------------

vcPath :: FilePath -> String -> FilePath
vcPath dir name = baseProjectFilePath dir name ++ "ViewController.swift"

appDelegatePath :: FilePath -> String -> FilePath
appDelegatePath dir name = baseProjectFilePath dir name ++ "AppDelegate.swift"

infoPath :: FilePath -> String -> FilePath
infoPath dir name = baseProjectFilePath dir name ++ "Info.plist"

pbxProjPath :: FilePath -> String -> FilePath
pbxProjPath dir name = pbxProjDirPath dir name ++ "project.pbxproject"

contentsPath :: FilePath -> String -> FilePath
contentsPath dir name = pbxProjDirPath dir name ++ ".xcworkspace/contents.xcworkspacedata"

baseProjectFilePath :: FilePath -> String -> FilePath
baseProjectFilePath dir name = dir ++ "/ios/" ++ name ++ "/"

pbxProjDirPath :: FilePath -> String -> FilePath
pbxProjDirPath dir name = dir ++ "/ios/" ++ name ++ ".xcodeproj/"
