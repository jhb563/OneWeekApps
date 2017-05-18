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

import           Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Template
import           System.Directory

import           Core.XCode.Templates.Other
import           Core.XCode.Templates.ProjectFile
import           Model.OWAAppInfo
import           Objc.Print (printStructureToFile)
import           Objc.XCode (initialVCHeader, initialVCImplementation, appDelegateHeader, appDelegateImplementation)
import           Swift.Print
import           Swift.XCode (initialVC, appDelegate)

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
  printSwiftFiles currentDirectory appInfo
  printObjcFiles currentDirectory appInfo

---------------------------------------------------------------------------
------------------------FILE PRINTERS--------------------------------------
---------------------------------------------------------------------------

printSwiftFiles :: FilePath -> OWAAppInfo -> IO ()
printSwiftFiles currentDirectory appInfo = do
  printSwiftVC currentDirectory appInfo
  printSwiftAppDelegate currentDirectory appInfo
  printInfo currentDirectory projName swiftDirName
  printContents currentDirectory projName cName swiftDirName
  printPbxProj currentDirectory projName cName swiftDirName
  where
    projName = appName appInfo
    cName = companyName appInfo
    swiftDirName = "swift"

printSwiftVC :: FilePath -> OWAAppInfo -> IO ()
printSwiftVC dir info = printSwiftStructureToFile
  (initialVC info) 
  (swiftVcPath dir (appName info)) 

printSwiftAppDelegate :: FilePath -> OWAAppInfo -> IO ()
printSwiftAppDelegate dir info = printSwiftStructureToFile
  (appDelegate info)
  (swiftAppDelegatePath dir (appName info))

printObjcFiles :: FilePath -> OWAAppInfo -> IO ()
printObjcFiles currentDirectory appInfo = do
  printObjcVCHeader currentDirectory appInfo
  printObjcVCM currentDirectory appInfo
  printObjcAppDelegateHeader currentDirectory appInfo
  printObjcAppDelegateM currentDirectory appInfo
  printInfo currentDirectory projName objcDirName
  printContents currentDirectory projName cName objcDirName
  printPbxProj currentDirectory projName cName objcDirName
  where
    projName = appName appInfo
    cName = companyName appInfo
    objcDirName = "objc"

printObjcVCHeader :: FilePath -> OWAAppInfo -> IO ()
printObjcVCHeader dir info = printStructureToFile
  (initialVCHeader info)
  (objcVcHeaderPath dir (appName info)) 

printObjcVCM :: FilePath -> OWAAppInfo -> IO ()
printObjcVCM dir info = printStructureToFile
  (initialVCImplementation info)
  (objcVcMPath dir (appName info)) 

printObjcAppDelegateHeader :: FilePath -> OWAAppInfo -> IO ()
printObjcAppDelegateHeader dir info = printStructureToFile
  (appDelegateHeader info)
  (objcAppDelegateHeaderPath dir (appName info))

printObjcAppDelegateM :: FilePath -> OWAAppInfo -> IO ()
printObjcAppDelegateM dir info = printStructureToFile
  (appDelegateImplementation info)
  (objcAppDelegateMPath dir (appName info))

printInfo :: FilePath -> String -> String -> IO ()
printInfo dir projName langType = do
  let fullPath = infoPath dir projName langType
  writeFile fullPath infoPListTemplate

printPbxProj :: FilePath -> String -> Maybe String -> String -> IO ()
printPbxProj dir projName cNameMaybe langType = writeFile fullPath (TL.unpack interpolatedText)
  where
    fullPath = pbxProjPath dir projName langType
    temp = template pbxProjTemplate
    interpolatedText = render temp (contextFunction projName cNameMaybe)

printContents :: FilePath -> String -> Maybe String -> String -> IO ()
printContents dir projName cNameMaybe langType = writeFile fullPath (TL.unpack interpolatedText)
  where
    fullPath = contentsPath dir projName langType
    temp = template contentsTemplate
    interpolatedText = render temp (contextFunction projName cNameMaybe)

-- Used for filling in variables
contextFunction :: String -> Maybe String -> T.Text -> T.Text
contextFunction projName _ "projectname" = T.pack projName 
contextFunction projName _ "projectbundlename" = replaceSpaces projName ""
  where
    -- Use Tail Recursion
    replaceSpaces :: String -> String -> T.Text
    replaceSpaces "" accum = T.pack $ reverse accum
    replaceSpaces (c : cs) accum = if c == ' '
      then replaceSpaces cs ('-' : accum)
      else replaceSpaces cs (c : accum)
contextFunction _ Nothing "companyname" = "oneweekapps"
contextFunction _ (Just cName) "companyname" = T.pack $ map toLower cNameNoSpaces
  where
    cNameNoSpaces = filter (/= ' ') cName
contextFunction _ _ txt = txt

---------------------------------------------------------------------------
------------------------DIRECTORIES AND FILES------------------------------
---------------------------------------------------------------------------

swiftVcPath :: FilePath -> String -> FilePath
swiftVcPath dir projName = baseProjectFilePath dir projName "swift" ++ "ViewController.swift"

swiftAppDelegatePath :: FilePath -> String -> FilePath
swiftAppDelegatePath dir projName = baseProjectFilePath dir projName "swift" ++ "AppDelegate.swift"

objcVcHeaderPath :: FilePath -> String -> FilePath
objcVcHeaderPath dir projName = baseProjectFilePath dir projName "objc" ++ "ViewController.h"

objcVcMPath :: FilePath -> String -> FilePath
objcVcMPath dir projName = baseProjectFilePath dir projName "objc" ++ "ViewController.m"

objcAppDelegateHeaderPath :: FilePath -> String -> FilePath
objcAppDelegateHeaderPath dir projName = baseProjectFilePath dir projName "objc" ++ "AppDelegate.h"

objcAppDelegateMPath :: FilePath -> String -> FilePath
objcAppDelegateMPath dir projName = baseProjectFilePath dir projName "objc" ++ "AppDelegate.m"

infoPath :: FilePath -> String -> String -> FilePath
infoPath dir projName langType = baseProjectFilePath dir projName langType ++ "Info.plist"

pbxProjPath :: FilePath -> String -> String -> FilePath
pbxProjPath dir projName langType = pbxProjDirPath dir projName langType ++ "project.pbxproj"

contentsPath :: FilePath -> String -> String -> FilePath
contentsPath dir projName langType = pbxProjDirPath dir projName langType ++ ".xcworkspace/contents.xcworkspacedata"

baseProjectFilePath :: FilePath -> String -> String -> FilePath
baseProjectFilePath dir projName langType = dir ++ "/" ++ langType ++ "/" ++ projName ++ "/"

pbxProjDirPath :: FilePath -> String -> String -> FilePath
pbxProjDirPath dir projName langType = dir ++ "/" ++ langType ++ "/" ++ projName ++ ".xcodeproj/"

directoriesToCreate :: FilePath -> String -> [FilePath]
directoriesToCreate dir projName =
  [ dir ++ "/swift/" ++ projName
  , dir ++ "/swift/" ++ projName ++ ".xcodeproj/.xcworkspace"
  , dir ++ "/objc/" ++ projName
  , dir ++ "/objc/" ++ projName ++ ".xcodeproj/.xcworkspace" ]
