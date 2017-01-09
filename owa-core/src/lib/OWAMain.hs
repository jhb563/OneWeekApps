{-|
Module      : OWAMain
Description : Main Library Entry point for OneWeekApps
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAMain (
  runOWA
) where

import Control.Exception (handle)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (runExceptT, ExceptT, ExceptT(..))
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Data.Either
import Data.List (find)
import Data.List.Split (splitOn)
import Control.Exception (handle)
import Data.Maybe (isJust, catMaybes, mapMaybe)
import Data.Time.Clock
import System.Directory
import System.IO 

import OWAAlertObjc
import OWAAlertParser
import OWAAlertSwift
import OWAAppInfoParser
import OWAColorObjc
import OWAColorParser
import OWAColorSwift
import OWAErrorObjc
import OWAErrorParser
import OWAErrorSwift
import Model.OWAAlert
import Model.OWAAppInfo
import Model.OWAColor
import Model.OWAError
import OWAExecTypes
import OWAFileNames
import OWAFileSearch
import OWAFontObjc
import OWAFontParser
import OWAFontSwift
import Model.OWAFont
import OWALazy
import Model.OWALocalizedStringSet
import OWANew
import OWAObjcPrint
import Model.OWAParseError
import OWAStringsObjc
import OWAStringsParser
import OWASwiftPrint
import OWATerminal
import OWAViewObjc
import OWAViewParser
import OWAViewSwift
import Model.OWAView
import OWAXCode
import System.Directory
import System.IO 

-- | 'runOWA' is the main running method for the OWA program. It takes a filepath
-- for a directory to search from, and generates all files.
runOWA :: Handle -> Handle -> FilePath -> [String] -> IO ()
runOWA iHandle oHandle filePath args = if null args
  then hPutStrLn oHandle "owa: No command entered!"
  else case head args of
    "new" -> runReaderT (runNewCommand filePath) initialRunnerContext
    "gen" -> genFiles
    "generate" -> genFiles
    unrecognizedCmd -> hPutStrLn oHandle $ "owa: unrecognized command \"" ++ unrecognizedCmd ++ "\"!"
    where
      types = codeTypesFromArgs args 
      lang = languageTypeFromArgs args
      initialRunnerContext = OWARunnerContext 
        (outputModeFromArgs $ tail args) 
        Nothing 
        Nothing 
        types 
        lang 
        iHandle 
        oHandle
      genFiles = do
                   (lastObjcTime, lastSwiftTime) <- lastCodeGenerationTime filePath
                   let newReaderInfo = initialRunnerContext 
                                        { lastObjcGenTime = lastObjcTime
                                        , lastSwiftGenTime = lastSwiftTime }
                   runReaderT (runOWAReader filePath) newReaderInfo

runOWAReader :: FilePath -> OWAReaderT ()
runOWAReader filePath = findAppDirectoryAndRun filePath generateFiles

generateFiles :: FilePath -> OWAReaderT ()
generateFiles appDirectory = do
  appInfo <- loadAppInfo appDirectory
  case appInfo of
    Nothing -> printIfNotSilent "Couldn't parse app info. Exiting."
    Just appInfo -> do
      produceColorsFiles appDirectory appInfo
      produceFontsFiles appDirectory appInfo
      produceAlertsFiles appDirectory appInfo
      produceErrorsFiles appDirectory appInfo
      produceStringsFile appDirectory appInfo
      produceViewsFiles appDirectory appInfo
      modifyLastGenTime appDirectory

findAppDirectoryAndRun :: FilePath -> (FilePath -> OWAReaderT ()) -> OWAReaderT ()
findAppDirectoryAndRun filePath action = do
  printIfNotSilent "Searching for app directory"
  maybeAppDirectory <- liftIO $ findAppDirectory filePath
  case maybeAppDirectory of 
    Nothing -> printIfNotSilent "Couldn't find app directory! Exiting"
    Just appDirectory -> do
      printIfNotSilent "Found app directory"
      action appDirectory

---------------------------------------------------------------------------
------------------------LOADING APP INFO-----------------------------------
---------------------------------------------------------------------------

loadAppInfo :: FilePath -> OWAReaderT (Maybe OWAAppInfo)
loadAppInfo appDirectory = do
  printIfNotSilent "Searching for app.info..."
  maybeAppFile <- liftIO $ findAppInfoFile appDirectory
  case maybeAppFile of
    Nothing -> do
      printIfNotSilent "Unable to find app.info. Please create an app.info file"
      return Nothing 
    Just appFile -> do
      printIfVerbose $ "Found app.info at " ++ appFile
      printIfNotSilent "Parsing app.info..."
      appInfoOrErrors <- liftIO $ parseAppInfoFromFile appFile
      case appInfoOrErrors of
        Left errors -> do
          printErrors errors 
          printIfNotSilent "Unable to parse app.info!"
          return Nothing
        Right appInfo -> do
          printIfNotSilent "Successfully parsed app.info!"
          return $ Just appInfo 

---------------------------------------------------------------------------
------------------------PRODUCING STRINGS FILES----------------------------
---------------------------------------------------------------------------

produceStringsFile :: FilePath -> OWAAppInfo -> OWAReaderT ()
produceStringsFile appDirectory appInfo = whenCodeTypePresent CodeTypeStrings $ do
  printIfNotSilent "Generating strings..."
  printIfVerbose "Searching for strings files..."
  stringsFiles <- liftIO $ findStringsFiles appDirectory
  printIfVerbose "Found strings files at: "
  mapM_ printIfVerbose stringsFiles
  regenFiles <- shouldRegenerateFromFiles ((appDirectory ++ appInfoFileExtension) : stringsFiles)
  if not regenFiles
    then do
      printIfNotSilent "No strings modifications since last generate!"
      printIfNotSilent "Skipping strings files generation!"
    else do
      listOfParseResults <- liftIO $ mapM parseStringsFromFile stringsFiles
      let errors = concat $ lefts listOfParseResults
      if not (null errors)
        then do
          printIfNotSilent "Encountered errors parsing strings..."
          printErrors errors
        else printIfVerbose "No errors parsing strings!"
      let stringSets = rights listOfParseResults
      printIfVerbose ("Successfully parsed " ++ show (length stringSets) ++ " sets of strings")
      let stringsFileStructure = objcStringsFileFromStringSets appInfo stringSets
      printIfVerbose "Printing strings file..."
      let fullStringsPath = appDirectory ++ "/../ios/" ++ appName appInfo ++ stringsFileExtension
      liftIO $ printStructureToFile stringsFileStructure fullStringsPath
      printIfVerbose "Printed strings to :" 
      printIfVerbose fullStringsPath
      printIfNotSilent "Finished generating strings!"

stringsFileExtension :: String
stringsFileExtension = "/Localizable.strings"

---------------------------------------------------------------------------
------------------------PRODUCING COLORS FILES-----------------------------
---------------------------------------------------------------------------

produceColorsFiles :: FilePath -> OWAAppInfo -> OWAReaderT ()
produceColorsFiles appDirectory appInfo = whenCodeTypePresent CodeTypeColors $ do
  printIfNotSilent "Generating colors..."
  printIfVerbose "Searching for colors files..."
  colorFiles <- liftIO $ findColorsFiles appDirectory
  printIfVerbose "Found colors files at: "
  mapM_ printIfVerbose colorFiles
  regenFiles <- shouldRegenerateFromFiles ((appDirectory ++ appInfoFileExtension) : colorFiles)
  if not regenFiles
    then do
      printIfNotSilent "No colors modifications since last generate!"
      printIfNotSilent "Skipping colors files generation!"
      return ()
    else do
      listOfParseResults <- liftIO $ mapM parseColorsFromFile colorFiles
      let errors = concat $ lefts listOfParseResults 
      if not (null errors)
        then do
          printIfNotSilent "Encountered errors parsing colors..."
          printErrors errors
        else printIfVerbose "No errors parsing colors!"
      let colors = concat $ rights listOfParseResults
      let prefix = appPrefix appInfo
      printIfVerbose ("Successfully parsed " ++ show (length colors) ++ " colors")
      lang <- languageType <$> ask
      producedFiles <- if lang == LanguageTypeObjc
        then do
          let colorHeaderFileStructure = objcHeaderFromColors appInfo colors
          let colorMFileStructure = objcImplementationFromColors appInfo colors
          printIfVerbose "Printing colors files..."
          let fullHeaderPath = appDirectory ++ "/../ios/" ++ appName appInfo ++ colorHeaderFileExtension prefix
          let fullMPath = appDirectory ++ "/../ios/" ++ appName appInfo ++ colorImplementationFileExtension prefix
          liftIO $ printStructureToFile colorHeaderFileStructure fullHeaderPath
          liftIO $ printStructureToFile colorMFileStructure fullMPath
          return $ fullHeaderPath ++ ", " ++ fullMPath
        else do
          let colorFileStructure = swiftExtensionFromColors appInfo colors
          printIfVerbose "Printing colors files..."
          let fullPath = appDirectory ++ "/../ios/" ++ appName appInfo ++ colorSwiftFileExtension prefix
          liftIO $ printSwiftStructureToFile colorFileStructure fullPath
          return fullPath
      printIfVerbose "Printed colors to files:"
      printIfVerbose producedFiles
      printIfNotSilent "Finished generating colors!"

colorHeaderFileExtension :: String -> FilePath
colorHeaderFileExtension prefix = "/UIColor+" ++ prefix ++ "Colors.h"

colorImplementationFileExtension :: String -> FilePath
colorImplementationFileExtension prefix = "/UIColor+" ++ prefix ++ "Colors.m"

colorSwiftFileExtension :: String -> FilePath
colorSwiftFileExtension prefix = "/UIColor+" ++ prefix ++ "Colors.swift"

---------------------------------------------------------------------------
------------------------PRODUCING FONTS FILES------------------------------
---------------------------------------------------------------------------

produceFontsFiles :: FilePath -> OWAAppInfo -> OWAReaderT ()
produceFontsFiles appDirectory appInfo = whenCodeTypePresent CodeTypeFonts $ do
  printIfNotSilent "Generating fonts..."
  printIfVerbose "Searching for fonts files..."
  fontFiles <- liftIO $ findFontsFiles appDirectory
  printIfVerbose "Found fonts files at: "
  mapM_ printIfVerbose fontFiles
  regenFiles <- shouldRegenerateFromFiles ((appDirectory ++ appInfoFileExtension) : fontFiles)
  if not regenFiles
    then do
      printIfNotSilent "No fonts modifications since last generate!"
      printIfNotSilent "Skipping fonts files generation!"
    else do
      listOfParseResults <- liftIO $ mapM parseFontsFromFile fontFiles
      let errors = concat $ lefts listOfParseResults 
      if not (null errors)
        then do
          printIfNotSilent "Encountered errors parsing fonts..."
          printErrors errors
        else printIfVerbose "No errors parsing fonts!"
      let fonts = concat $ rights listOfParseResults 
      let prefix = appPrefix appInfo
      printIfVerbose ("Found " ++ show (length fonts) ++ " fonts")
      lang <- languageType <$> ask
      producedFiles <- if lang == LanguageTypeObjc
        then do
          let fontHeaderFileStructure = objcHeaderFromFonts appInfo fonts
          let fontMFileStructure = objcImplementationFromFonts appInfo fonts
          printIfVerbose "Printing fonts files..."
          let fullHeaderPath = appDirectory ++ "/../ios/" ++ appName appInfo ++ fontHeaderFileExtension prefix
          let fullMPath = appDirectory ++ "/../ios/" ++ appName appInfo ++ fontImplementationFileExtension prefix
          liftIO $ printStructureToFile fontHeaderFileStructure fullHeaderPath
          liftIO $ printStructureToFile fontMFileStructure fullMPath
          return $ fullHeaderPath ++ ", " ++ fullMPath
        else do
          let fontFileStructure = swiftExtensionFromFonts appInfo fonts
          printIfVerbose "Printing fonts files..."
          let fullPath = appDirectory ++ "/../ios/" ++ appName appInfo ++ fontSwiftFileExtension prefix
          liftIO $ printSwiftStructureToFile fontFileStructure fullPath
          return fullPath
      printIfVerbose "Printed fonts to files:"
      printIfVerbose producedFiles 
      printIfNotSilent "Finished generating fonts!"

fontHeaderFileExtension :: String -> FilePath
fontHeaderFileExtension prefix = "/UIFont+" ++ prefix ++ "Fonts.h"

fontImplementationFileExtension :: String -> FilePath
fontImplementationFileExtension prefix = "/UIFont+" ++ prefix ++ "Fonts.m"

fontSwiftFileExtension :: String -> FilePath
fontSwiftFileExtension prefix = "/UIFont+" ++ prefix ++ "Fonts.swift"

---------------------------------------------------------------------------
------------------------PRODUCING ALERTS FILES-----------------------------
---------------------------------------------------------------------------

produceAlertsFiles :: FilePath -> OWAAppInfo -> OWAReaderT ()
produceAlertsFiles appDirectory appInfo = whenCodeTypePresent CodeTypeAlerts $ do
  printIfNotSilent "Generating alerts..."
  printIfVerbose "Searching for alerts files..."
  alertFiles <- liftIO $ findAlertsFiles appDirectory
  printIfVerbose "Found alerts files at: "
  mapM_ printIfVerbose alertFiles
  regenFiles <- shouldRegenerateFromFiles ((appDirectory ++ appInfoFileExtension) : alertFiles)
  if not regenFiles
    then do
      printIfNotSilent "No alerts modifications since last generate!"
      printIfNotSilent "Skipping alerts files generation!"
    else do
      listOfParseResults <- liftIO $ mapM parseAlertsFromFile alertFiles
      let errors = concat $ lefts listOfParseResults 
      if not (null errors)
        then do
          printIfNotSilent "Encountered errors parsing alerts..."
          printErrors errors
        else printIfVerbose "No errors parsing alerts!"
      let alerts = concat $ rights listOfParseResults
      let prefix = appPrefix appInfo
      printIfVerbose ("Found " ++ show (length alerts) ++ " alerts")
      lang <- languageType <$> ask
      producedFiles <- if lang == LanguageTypeObjc
        then do
          let alertHeaderFileStructure = objcHeaderFromAlerts appInfo alerts
          let alertMFileStructure = objcImplementationFromAlerts appInfo alerts
          printIfVerbose "Printing alerts files..."
          let fullHeaderPath = appDirectory ++ "/../ios/" ++ appName appInfo ++ alertHeaderFileExtension prefix
          let fullMPath = appDirectory ++ "/../ios/" ++ appName appInfo ++ alertImplementationFileExtension prefix
          liftIO $ printStructureToFile alertHeaderFileStructure fullHeaderPath
          liftIO $ printStructureToFile alertMFileStructure fullMPath
          return $ fullHeaderPath ++ ", " ++ fullMPath
        else do
          let alertFileStructure = swiftExtensionFromAlerts appInfo alerts
          printIfVerbose "Printing alerts files..."
          let fullPath = appDirectory ++ "/../ios/" ++ appName appInfo ++ alertSwiftFileExtension prefix
          liftIO $ printSwiftStructureToFile alertFileStructure fullPath
          return fullPath
      printIfVerbose "Printed alerts to files:"
      printIfVerbose producedFiles
      printIfNotSilent "Finished generating alerts!"

alertHeaderFileExtension :: String -> FilePath
alertHeaderFileExtension prefix = "/UIAlertController+" ++ prefix ++ "Alerts.h"

alertImplementationFileExtension :: String -> FilePath
alertImplementationFileExtension prefix = "/UIAlertController+" ++ prefix ++ "Alerts.m"

alertSwiftFileExtension :: String -> FilePath
alertSwiftFileExtension prefix = "/UIAlertController+" ++ prefix ++ "Alerts.swift"

---------------------------------------------------------------------------
------------------------PRODUCING ERRORS FILES-----------------------------
---------------------------------------------------------------------------

produceErrorsFiles :: FilePath -> OWAAppInfo -> OWAReaderT ()
produceErrorsFiles appDirectory appInfo = whenCodeTypePresent CodeTypeErrors $ do
  printIfNotSilent "Generating errors..."
  printIfVerbose "Searching for errors files..."
  errorFiles <- liftIO $ findErrorsFiles appDirectory
  printIfVerbose "Found errors files at: "
  mapM_ printIfVerbose errorFiles
  regenFiles <- shouldRegenerateFromFiles ((appDirectory ++ appInfoFileExtension) : errorFiles)
  if not regenFiles
    then do
      printIfNotSilent "No errors modifications since last generate!"
      printIfNotSilent "Skipping errors files generation!"
    else do
      listOfParseResults <- liftIO $ mapM parseErrorsFromFile errorFiles
      let errors = concat $ lefts listOfParseResults 
      if not (null errors)
        then do
          printIfNotSilent "Encountered errors parsing errors..."
          printErrors errors
        else printIfVerbose "No errors parsing errors!"
      let errors = concat $ rights listOfParseResults
      let prefix = appPrefix appInfo
      printIfVerbose ("Found " ++ show (length errors) ++ " errors")
      lang <- languageType <$> ask
      producedFiles <- if lang == LanguageTypeObjc
        then do
          let errorHeaderFileStructure = objcHeaderFromErrors appInfo errors
          let errorMFileStructure = objcImplementationFromErrors appInfo errors
          printIfVerbose "Printing errors files..."
          let fullHeaderPath = appDirectory ++ "/../ios/" ++ appName appInfo ++ errorHeaderFileExtension prefix
          let fullMPath = appDirectory ++ "/../ios/" ++ appName appInfo ++ errorImplementationFileExtension prefix
          liftIO $ printStructureToFile errorHeaderFileStructure fullHeaderPath
          liftIO $ printStructureToFile errorMFileStructure fullMPath
          return $ fullHeaderPath ++ ", " ++ fullMPath
        else do
          let errorFileStructure = swiftExtensionFromErrors appInfo errors
          printIfVerbose "Printing errors files..."
          let fullPath = appDirectory ++ "/../ios/" ++ appName appInfo ++ errorSwiftFileExtension prefix
          liftIO $ printSwiftStructureToFile errorFileStructure fullPath
          return fullPath
      printIfVerbose "Printed errors to files:"
      printIfVerbose producedFiles 
      printIfNotSilent "Finished generating errors!"

errorHeaderFileExtension :: String -> FilePath
errorHeaderFileExtension prefix = "/NSError+" ++ prefix ++ "Errors.h"

errorImplementationFileExtension :: String -> FilePath
errorImplementationFileExtension prefix = "/NSError+" ++ prefix ++ "Errors.m"

errorSwiftFileExtension :: String -> FilePath
errorSwiftFileExtension prefix = "/NSError+" ++ prefix ++ "Errors.swift"

---------------------------------------------------------------------------
------------------------PRODUCING VIEWS FILES------------------------------
---------------------------------------------------------------------------

produceViewsFiles :: FilePath -> OWAAppInfo -> OWAReaderT ()
produceViewsFiles appDirectory appInfo = whenCodeTypePresent CodeTypeViews $ do
  printIfNotSilent "Generating views..."
  printIfVerbose "Searching for views files..."
  viewFiles <- liftIO $ findViewsFiles appDirectory
  printIfVerbose "Found view files at: "
  mapM_ printIfVerbose viewFiles
  let appInfoFile = appDirectory ++ appInfoFileExtension
  viewFilesToRegen <- filterM (\v -> shouldRegenerateFromFiles [appInfoFile, v]) viewFiles
  listOfParseResults <- liftIO $ mapM parseViewFromFile viewFilesToRegen
  let errors = concat $ lefts listOfParseResults
  if not (null errors)
    then do
      printIfNotSilent "Encountered errors parsing views..."
      printErrors errors
    else printIfVerbose "No errors parsing views!"
  let views = rights listOfParseResults
  printIfVerbose ("Found" ++ show (length views) ++ " views")
  if not (null views)
    then printIfVerbose "Printing views..."
    else printIfVerbose "All views are up to date!"
  mapM_ (printViewFiles appDirectory appInfo) views
  printIfNotSilent "Finished generating views!"
   
printViewFiles :: FilePath -> OWAAppInfo -> OWAView -> OWAReaderT ()
printViewFiles appDirectory appInfo view = do
  lang <- languageType <$> ask
  if lang == LanguageTypeObjc
    then do
      liftIO $ printStructureToFile headerStructure headerPath
      liftIO $ printStructureToFile mStructure mPath
      printIfVerbose headerPath
      printIfVerbose mPath
    else do
      liftIO $ printSwiftStructureToFile swiftStructure swiftPath
      printIfVerbose swiftPath
  where 
    headerStructure = objcHeaderFromView appInfo view
    mStructure = objcImplementationFromView appInfo view
    vTy = viewType view
    headerPath = appDirectory ++ "/../ios/" ++ appName appInfo ++ '/':vTy ++ ".h"
    mPath = appDirectory ++ "/../ios/" ++ appName appInfo ++ '/':vTy ++ ".m"
    swiftStructure = swiftFileFromView appInfo view
    swiftPath = appDirectory ++ "/../ios/" ++ appName appInfo ++ '/':vTy ++ ".swift"
