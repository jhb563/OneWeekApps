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
import Data.Either
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
import OWAObjcPrint
import OWAParseError

-- | 'runOWA' is the main running method for the OWA program. It takes a filepath
-- for a directory to search from, and generates all files.
runOWA :: FilePath -> [String] -> IO ()
runOWA filePath args = do
  let outputMode = outputModeFromArgs args
  printIfNotSilent outputMode ("Searching For app directory from " ++ filePath)
  maybeAppDirectory <- findAppDirectory filePath
  case maybeAppDirectory of 
    Nothing -> printIfNotSilent outputMode "Couldn't find app directory! Exiting"
    Just appDirectory -> do
      printIfNotSilent outputMode ("Found app directory at " ++ appDirectory) 
      appInfo <- loadAppInfo outputMode appDirectory
      case appInfo of
        Nothing -> printIfNotSilent outputMode "Exiting."
        Just appInfo -> do
          produceColorsFiles outputMode appDirectory appInfo
          produceFontsFiles outputMode appDirectory appInfo
          produceAlertsFiles outputMode appDirectory appInfo
          produceErrorsFiles outputMode appDirectory appInfo

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

printIfNotSilent :: OutputMode -> String -> IO ()
printIfNotSilent mode str = Control.Monad.when (mode /= Silent) $ putStrLn str

printIfVerbose :: OutputMode -> String -> IO ()
printIfVerbose mode str = Control.Monad.when (mode == Verbose) $ putStrLn str

printErrors :: OutputMode -> [OWAParseError] -> IO ()
printErrors outputMode [] = return ()
printErrors outputMode errors = mapM_ (printIfNotSilent outputMode . show) errors

---------------------------------------------------------------------------
------------------------LOADING APP INFO-----------------------------------
---------------------------------------------------------------------------

loadAppInfo :: OutputMode -> FilePath -> IO (Maybe OWAAppInfo)
loadAppInfo outputMode appDirectory = do
  printIfNotSilent outputMode "Searching for app.info..."
  maybeAppFile <- findAppInfoFile appDirectory
  case maybeAppFile of
    Nothing -> do
      printIfNotSilent outputMode "Unable to find app.info. Please create an app.info file"
      return Nothing 
    Just appFile -> do
      printIfVerbose outputMode $ "Found app.info at " ++ appFile
      printIfNotSilent outputMode "Parsing app.info..."
      appInfoOrErrors <- parseAppInfoFromFile appFile
      case appInfoOrErrors of
        Left errors -> do
          printErrors outputMode errors 
          printIfNotSilent outputMode "Unable to parse app.info!"
          return Nothing
        Right appInfo -> do
          printIfNotSilent outputMode "Successfully parsed app.info!"
          return $ Just appInfo 

---------------------------------------------------------------------------
------------------------PRODUCING COLORS FILES-----------------------------
---------------------------------------------------------------------------

produceColorsFiles :: OutputMode -> FilePath -> OWAAppInfo -> IO ()
produceColorsFiles outputMode appDirectory appInfo = do
  printIfNotSilent outputMode "Generating colors..."
  printIfVerbose outputMode "Searching for colors files..."
  colorFiles <- findColorsFiles appDirectory
  printIfVerbose outputMode "Found colors files at: "
  mapM_ (printIfVerbose outputMode) colorFiles
  listOfParseResults <- mapM parseColorsFromFile colorFiles
  let errors = concat $ lefts listOfParseResults 
  if not (null errors)
    then do
      printIfNotSilent outputMode "Encountered errors parsing colors..."
      printErrors outputMode errors
    else printIfVerbose outputMode "No errors parsing colors!"
  let colors = concat $ rights listOfParseResults
  let prefix = appPrefix appInfo
  printIfVerbose outputMode ("Successfully parsed " ++ show (length colors) ++ " colors")
  let colorHeaderFileStructure = objcHeaderFromColors appInfo colors
  let colorMFileStructure = objcImplementationFromColors appInfo colors
  printIfVerbose outputMode "Printing colors files..."
  let fullHeaderPath = appDirectory ++ colorHeaderFileExtension prefix
  let fullMPath = appDirectory ++ colorImplementationFileExtension prefix
  printStructureToFile colorHeaderFileStructure fullHeaderPath
  printStructureToFile colorMFileStructure fullMPath
  printIfVerbose outputMode "Printed colors to files:"
  printIfVerbose outputMode (fullHeaderPath ++ ", " ++ fullMPath)
  printIfNotSilent outputMode "Finished generating colors!"

colorHeaderFileExtension :: String -> FilePath
colorHeaderFileExtension prefix = "/UIColor+" ++ prefix ++ "Colors.h"

colorImplementationFileExtension :: String -> FilePath
colorImplementationFileExtension prefix = "/UIColor+" ++ prefix ++ "Colors.m"

---------------------------------------------------------------------------
------------------------PRODUCING FONTS FILES------------------------------
---------------------------------------------------------------------------

produceFontsFiles :: OutputMode -> FilePath -> OWAAppInfo -> IO ()
produceFontsFiles outputMode appDirectory appInfo = do
  printIfNotSilent outputMode "Generating fonts..."
  printIfVerbose outputMode "Searching for fonts files..."
  fontFiles <- findFontsFiles appDirectory
  printIfVerbose outputMode "Found fonts files at: "
  mapM_ (printIfVerbose outputMode) fontFiles
  listOfParseResults <- mapM parseFontsFromFile fontFiles
  let errors = concat $ lefts listOfParseResults 
  if not (null errors)
    then do
      printIfNotSilent outputMode "Encountered errors parsing fonts..."
      printErrors outputMode errors
    else printIfVerbose outputMode "No errors parsing fonts!"
  let fonts = concat $ rights listOfParseResults 
  let prefix = appPrefix appInfo
  printIfVerbose outputMode ("Found " ++ show (length fonts) ++ " fonts")
  let fontHeaderFileStructure = objcHeaderFromFonts appInfo fonts
  let fontMFileStructure = objcImplementationFromFonts appInfo fonts
  printIfVerbose outputMode "Printing fonts files..."
  let fullHeaderPath = appDirectory ++ fontHeaderFileExtension prefix
  let fullMPath = appDirectory ++ fontImplementationFileExtension prefix
  printStructureToFile fontHeaderFileStructure fullHeaderPath
  printStructureToFile fontMFileStructure fullMPath
  printIfVerbose outputMode "Printed fonts to files:"
  printIfVerbose outputMode (fullHeaderPath ++ ", " ++ fullMPath)
  printIfNotSilent outputMode "Finished generating fonts!"

fontHeaderFileExtension :: String -> FilePath
fontHeaderFileExtension prefix = "/UIFont+" ++ prefix ++ "Fonts.h"

fontImplementationFileExtension :: String -> FilePath
fontImplementationFileExtension prefix = "/UIFont+" ++ prefix ++ "Fonts.m"

---------------------------------------------------------------------------
------------------------PRODUCING ALERTS FILES-----------------------------
---------------------------------------------------------------------------

produceAlertsFiles :: OutputMode -> FilePath -> OWAAppInfo -> IO ()
produceAlertsFiles outputMode appDirectory appInfo = do
  printIfNotSilent outputMode "Generating alerts..."
  printIfVerbose outputMode "Searching for alerts files..."
  alertFiles <- findAlertsFiles appDirectory
  printIfVerbose outputMode "Found alerts files at: "
  mapM_ (printIfVerbose outputMode) alertFiles
  listOfParseResults <- mapM parseAlertsFromFile alertFiles
  let errors = concat $ lefts listOfParseResults 
  if not (null errors)
    then do
      printIfNotSilent outputMode "Encountered errors parsing alerts..."
      printErrors outputMode errors
    else printIfVerbose outputMode "No errors parsing alerts!"
  let alerts = concat $ rights listOfParseResults
  let prefix = appPrefix appInfo
  printIfVerbose outputMode ("Found " ++ show (length alerts) ++ " alerts")
  let alertHeaderFileStructure = objcHeaderFromAlerts appInfo alerts
  let alertMFileStructure = objcImplementationFromAlerts appInfo alerts
  printIfVerbose outputMode "Printing alerts files..."
  let fullHeaderPath = appDirectory ++ alertHeaderFileExtension prefix
  let fullMPath = appDirectory ++ alertImplementationFileExtension prefix
  printStructureToFile alertHeaderFileStructure fullHeaderPath
  printStructureToFile alertMFileStructure fullMPath
  printIfVerbose outputMode "Printed alerts to files:"
  printIfVerbose outputMode (fullHeaderPath ++ ", " ++ fullMPath)
  printIfNotSilent outputMode "Finished generating alerts!"

alertHeaderFileExtension :: String -> FilePath
alertHeaderFileExtension prefix = "/UIAlertController+" ++ prefix ++ "Alerts.h"

alertImplementationFileExtension :: String -> FilePath
alertImplementationFileExtension prefix = "/UIAlertController+" ++ prefix ++ "Alerts.m"

---------------------------------------------------------------------------
------------------------PRODUCING ERRORS FILES-----------------------------
---------------------------------------------------------------------------

produceErrorsFiles :: OutputMode -> FilePath -> OWAAppInfo -> IO ()
produceErrorsFiles outputMode appDirectory appInfo = do
  printIfNotSilent outputMode "Generating errors..."
  printIfVerbose outputMode "Searching for errors files..."
  errorFiles <- findErrorsFiles appDirectory
  printIfVerbose outputMode "Found errors files at: "
  mapM_ (printIfVerbose outputMode) errorFiles
  listOfParseResults <- mapM parseErrorsFromFile errorFiles
  let errors = concat $ lefts listOfParseResults 
  if not (null errors)
    then do
      printIfNotSilent outputMode "Encountered errors parsing errors..."
      printErrors outputMode errors
    else printIfVerbose outputMode "No errors parsing errors!"
  let errors = concat $ rights listOfParseResults
  let prefix = appPrefix appInfo
  printIfVerbose outputMode ("Found " ++ show (length errors) ++ " errors")
  let errorHeaderFileStructure = objcHeaderFromErrors appInfo errors
  let errorMFileStructure = objcImplementationFromErrors appInfo errors
  printIfVerbose outputMode "Printing errors files..."
  let fullHeaderPath = appDirectory ++ errorHeaderFileExtension prefix
  let fullMPath = appDirectory ++ errorImplementationFileExtension prefix
  printStructureToFile errorHeaderFileStructure fullHeaderPath
  printStructureToFile errorMFileStructure fullMPath
  printIfVerbose outputMode "Printed errors to files:"
  printIfVerbose outputMode (fullHeaderPath ++ ", " ++ fullMPath)
  printIfNotSilent outputMode "Finished generating errors!"

errorHeaderFileExtension :: String -> FilePath
errorHeaderFileExtension prefix = "/NSError+" ++ prefix ++ "Errors.h"

errorImplementationFileExtension :: String -> FilePath
errorImplementationFileExtension prefix = "/NSError+" ++ prefix ++ "Errors.m"
