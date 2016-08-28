{-|
Module      : OWALib
Description : Main Library Entry point for OneWeekApps
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWALib (
  runOWA
) where

import Control.Monad
import Control.Monad.Reader
import Data.Either
import Data.Time.Clock
import OWAAlert
import OWAAlertObjc
import OWAAlertParser
import OWAAppInfo
import OWAAppInfoParser
import OWAColor
import OWAColorObjc
import OWAColorParser
import OWAError
import OWAErrorObjc
import OWAErrorParser
import OWAFileSearch
import OWAFont
import OWAFontObjc
import OWAFontParser
import OWALocalizedStringSet
import OWAObjcPrint
import OWAParseError
import OWAStringsParser
import OWAStringsObjc
import OWAView
import OWAViewObjc
import OWAViewParser
import System.Directory
import System.IO

type OWAReaderT = ReaderT OWAReaderInfo IO

data OWAReaderInfo = OWAReaderInfo {
  outputMode  :: OutputMode,
  lastGenTime :: Maybe UTCTime
}

-- | 'runOWA' is the main running method for the OWA program. It takes a filepath
-- for a directory to search from, and generates all files.
runOWA :: FilePath -> [String] -> IO ()
runOWA filePath args = if null args
  then putStrLn "owa: No command entered!"
  else case head args of
    "new" -> putStrLn "Creating new OWA project!"
    "gen" -> genFiles
    "generate" -> genFiles
    unrecognizedCmd -> putStrLn  $ "owa: unrecognized command \"" ++ unrecognizedCmd ++ "\"!"
    where initialReaderInfo = OWAReaderInfo (outputModeFromArgs $ tail args) Nothing
          genFiles = do
                       lastGenTime <- lastCodeGenerationTime filePath
                       let newReaderInfo = initialReaderInfo {lastGenTime = lastGenTime}
                       runReaderT (runOWAReader filePath) newReaderInfo

runOWAReader :: FilePath -> OWAReaderT ()
runOWAReader filePath = do
  printIfNotSilent ("Searching For app directory from " ++ filePath)
  maybeAppDirectory <- liftIO $ findAppDirectory filePath
  case maybeAppDirectory of 
    Nothing -> printIfNotSilent "Couldn't find app directory! Exiting"
    Just appDirectory -> do
      printIfNotSilent ("Found app directory at " ++ appDirectory) 
      appInfo <- loadAppInfo appDirectory
      case appInfo of
        Nothing -> printIfNotSilent "Exiting."
        Just appInfo -> do
          produceColorsFiles appDirectory appInfo
          produceFontsFiles appDirectory appInfo
          produceAlertsFiles appDirectory appInfo
          produceErrorsFiles appDirectory appInfo
          produceStringsFile appDirectory appInfo
          produceViewsFiles appDirectory appInfo
          liftIO $ modifyLastGenTime filePath

---------------------------------------------------------------------------
------------------------LAZY CODE GENERATION HANDLERS----------------------
---------------------------------------------------------------------------

lastCodeGenerationTime :: FilePath -> IO (Maybe UTCTime)
lastCodeGenerationTime filePath = do
  let lastGenFilePath = filePath ++ lastGenFileExtension
  lastGenExists <- doesFileExist lastGenFilePath
  if lastGenExists
    then do
      lastModified <- getModificationTime lastGenFilePath
      return $ Just lastModified
    else return Nothing

modifyLastGenTime :: FilePath -> IO ()
modifyLastGenTime filePath = do
  let lastGenFilePath = filePath ++ lastGenFileExtension
  lastModFileExists <- doesFileExist lastGenFilePath
  if lastModFileExists
    then do
      currentTime <- getCurrentTime
      setModificationTime lastGenFilePath currentTime
    else do
      handle <- openFile lastGenFilePath WriteMode
      hClose handle
  
lastGenFileExtension :: FilePath
lastGenFileExtension = "/.owa_last_gen"

appInfoFileExtension :: FilePath
appInfoFileExtension = "/app.info"

shouldRegenerateFromFiles :: [FilePath] -> OWAReaderT Bool
shouldRegenerateFromFiles sourceFiles = do
  info <- ask
  let genTime = lastGenTime info
  sourceTimes <- liftIO $ mapM getModificationTime sourceFiles
  case genTime of
    Nothing -> return True
    Just time -> return $ any (time <) sourceTimes

---------------------------------------------------------------------------
------------------------PROGRAM STATUS PRINTING----------------------------
---------------------------------------------------------------------------

data OutputMode = Silent | Normal | Verbose deriving (Show, Eq)

outputModeFromArgs :: [String] -> OutputMode
outputModeFromArgs args 
  | silentMode = Silent
  | verboseMode = Verbose
  | otherwise = Normal 
    where silentMode = elem "-silent" args || elem "-s" args
          verboseMode = elem "-verbose" args || elem "-v" args

printIfNotSilent :: String -> OWAReaderT ()
printIfNotSilent str = do
  info <- ask
  let mode = outputMode info
  Control.Monad.when (mode /= Silent) $ liftIO $ putStrLn str

printIfVerbose :: String -> OWAReaderT ()
printIfVerbose str = do
  info <- ask
  let mode = outputMode info
  Control.Monad.when (mode == Verbose) $ liftIO $ putStrLn str

printErrors :: [OWAParseError] -> OWAReaderT ()
printErrors [] = return ()
printErrors errors = mapM_ (printIfNotSilent . show) errors

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
produceStringsFile appDirectory appInfo = do
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
      let fullStringsPath = appDirectory ++ stringsFileExtension
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
produceColorsFiles appDirectory appInfo = do
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
      let colorHeaderFileStructure = objcHeaderFromColors appInfo colors
      let colorMFileStructure = objcImplementationFromColors appInfo colors
      printIfVerbose "Printing colors files..."
      let fullHeaderPath = appDirectory ++ colorHeaderFileExtension prefix
      let fullMPath = appDirectory ++ colorImplementationFileExtension prefix
      liftIO $ printStructureToFile colorHeaderFileStructure fullHeaderPath
      liftIO $ printStructureToFile colorMFileStructure fullMPath
      printIfVerbose "Printed colors to files:"
      printIfVerbose (fullHeaderPath ++ ", " ++ fullMPath)
      printIfNotSilent "Finished generating colors!"

colorHeaderFileExtension :: String -> FilePath
colorHeaderFileExtension prefix = "/UIColor+" ++ prefix ++ "Colors.h"

colorImplementationFileExtension :: String -> FilePath
colorImplementationFileExtension prefix = "/UIColor+" ++ prefix ++ "Colors.m"

---------------------------------------------------------------------------
------------------------PRODUCING FONTS FILES------------------------------
---------------------------------------------------------------------------

produceFontsFiles :: FilePath -> OWAAppInfo -> OWAReaderT ()
produceFontsFiles appDirectory appInfo = do
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
      let fontHeaderFileStructure = objcHeaderFromFonts appInfo fonts
      let fontMFileStructure = objcImplementationFromFonts appInfo fonts
      printIfVerbose "Printing fonts files..."
      let fullHeaderPath = appDirectory ++ fontHeaderFileExtension prefix
      let fullMPath = appDirectory ++ fontImplementationFileExtension prefix
      liftIO $ printStructureToFile fontHeaderFileStructure fullHeaderPath
      liftIO $ printStructureToFile fontMFileStructure fullMPath
      printIfVerbose "Printed fonts to files:"
      printIfVerbose (fullHeaderPath ++ ", " ++ fullMPath)
      printIfNotSilent "Finished generating fonts!"

fontHeaderFileExtension :: String -> FilePath
fontHeaderFileExtension prefix = "/UIFont+" ++ prefix ++ "Fonts.h"

fontImplementationFileExtension :: String -> FilePath
fontImplementationFileExtension prefix = "/UIFont+" ++ prefix ++ "Fonts.m"

---------------------------------------------------------------------------
------------------------PRODUCING ALERTS FILES-----------------------------
---------------------------------------------------------------------------

produceAlertsFiles :: FilePath -> OWAAppInfo -> OWAReaderT ()
produceAlertsFiles appDirectory appInfo = do
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
      let alertHeaderFileStructure = objcHeaderFromAlerts appInfo alerts
      let alertMFileStructure = objcImplementationFromAlerts appInfo alerts
      printIfVerbose "Printing alerts files..."
      let fullHeaderPath = appDirectory ++ alertHeaderFileExtension prefix
      let fullMPath = appDirectory ++ alertImplementationFileExtension prefix
      liftIO $ printStructureToFile alertHeaderFileStructure fullHeaderPath
      liftIO $ printStructureToFile alertMFileStructure fullMPath
      printIfVerbose "Printed alerts to files:"
      printIfVerbose (fullHeaderPath ++ ", " ++ fullMPath)
      printIfNotSilent "Finished generating alerts!"

alertHeaderFileExtension :: String -> FilePath
alertHeaderFileExtension prefix = "/UIAlertController+" ++ prefix ++ "Alerts.h"

alertImplementationFileExtension :: String -> FilePath
alertImplementationFileExtension prefix = "/UIAlertController+" ++ prefix ++ "Alerts.m"

---------------------------------------------------------------------------
------------------------PRODUCING ERRORS FILES-----------------------------
---------------------------------------------------------------------------

produceErrorsFiles :: FilePath -> OWAAppInfo -> OWAReaderT ()
produceErrorsFiles appDirectory appInfo = do
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
      let errorHeaderFileStructure = objcHeaderFromErrors appInfo errors
      let errorMFileStructure = objcImplementationFromErrors appInfo errors
      printIfVerbose "Printing errors files..."
      let fullHeaderPath = appDirectory ++ errorHeaderFileExtension prefix
      let fullMPath = appDirectory ++ errorImplementationFileExtension prefix
      liftIO $ printStructureToFile errorHeaderFileStructure fullHeaderPath
      liftIO $ printStructureToFile errorMFileStructure fullMPath
      printIfVerbose "Printed errors to files:"
      printIfVerbose (fullHeaderPath ++ ", " ++ fullMPath)
      printIfNotSilent "Finished generating errors!"

errorHeaderFileExtension :: String -> FilePath
errorHeaderFileExtension prefix = "/NSError+" ++ prefix ++ "Errors.h"

errorImplementationFileExtension :: String -> FilePath
errorImplementationFileExtension prefix = "/NSError+" ++ prefix ++ "Errors.m"

---------------------------------------------------------------------------
------------------------PRODUCING VIEWS FILES------------------------------
---------------------------------------------------------------------------

produceViewsFiles :: FilePath -> OWAAppInfo -> OWAReaderT ()
produceViewsFiles appDirectory appInfo = do
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
  liftIO $ printStructureToFile headerStructure headerPath
  liftIO $ printStructureToFile mStructure mPath
  printIfVerbose headerPath
  printIfVerbose mPath
    where headerStructure = objcHeaderFromView appInfo view
          mStructure = objcImplementationFromView appInfo view
          vTy = viewType view
          headerPath = appDirectory ++ '/':vTy ++ ".h"
          mPath = appDirectory ++ '/':vTy ++ ".m"
