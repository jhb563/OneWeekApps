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

import Data.Either
import OWAAlert
import OWAAlertObjc
import OWAAlertParser
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
      produceColorsFiles outputMode appDirectory
      produceFontsFiles outputMode appDirectory
      produceAlertsFiles outputMode appDirectory
      produceErrorsFiles outputMode appDirectory

---------------------------------------------------------------------------
------------------------PROGRAM STATUS PRINTING----------------------------
---------------------------------------------------------------------------

data OutputMode = Silent | Normal | Verbose deriving (Show, Eq)

outputModeFromArgs :: [String] -> OutputMode
outputModeFromArgs args = if silentMode 
  then Silent
  else if verboseMode
    then Verbose
    else Normal
    where silentMode = elem "-silent" args || elem "-s" args
          verboseMode = elem "-verbose" args || elem "-v" args

printIfNotSilent :: OutputMode -> String -> IO ()
printIfNotSilent mode str = if mode /= Silent
  then putStrLn str
  else return ()

printIfVerbose :: OutputMode -> String -> IO ()
printIfVerbose mode str = if mode == Verbose
  then putStrLn str
  else return ()

---------------------------------------------------------------------------
------------------------PRODUCING COLORS FILES-----------------------------
---------------------------------------------------------------------------

produceColorsFiles :: OutputMode -> FilePath -> IO ()
produceColorsFiles outputMode appDirectory = do
  printIfNotSilent outputMode "Generating colors..."
  printIfVerbose outputMode "Searching for colors files..."
  colorFiles <- findColorsFiles appDirectory
  printIfVerbose outputMode "Found colors files at: "
  mapM_ (printIfVerbose outputMode) colorFiles
  listOfColorLists <- mapM parseColorsFromFile colorFiles
  let colors = concat $ rights listOfColorLists
  printIfVerbose outputMode ("Found " ++ (show $ length colors) ++ (" colors"))
  let colorHeaderFileStructure = objcHeaderFromColors colorCategoryName colors
  let colorMFileStructure = objcImplementationFromColors colorCategoryName colors
  printIfVerbose outputMode "Printing colors files..."
  let fullHeaderPath = appDirectory ++ fontHeaderFileExtension
  let fullMPath = appDirectory ++ fontImplementationFileExtension
  printStructureToFile colorHeaderFileStructure fullHeaderPath
  printStructureToFile colorMFileStructure fullMPath
  printIfVerbose outputMode "Printed colors to files:"
  printIfVerbose outputMode (fullHeaderPath ++ ", " ++ fullMPath)
  printIfNotSilent outputMode "Finished generating colors!"

colorCategoryName :: String
colorCategoryName = "MyAppColors"

colorHeaderFileExtension :: FilePath
colorHeaderFileExtension = "/UIColor+MyAppColors.h"

colorImplementationFileExtension :: FilePath
colorImplementationFileExtension = "/UIColor+MyAppColors.m"

---------------------------------------------------------------------------
------------------------PRODUCING FONTS FILES------------------------------
---------------------------------------------------------------------------

produceFontsFiles :: OutputMode -> FilePath -> IO ()
produceFontsFiles outputMode appDirectory = do
  printIfNotSilent outputMode "Generating fonts..."
  printIfVerbose outputMode "Searching for fonts files..."
  fontFiles <- findFontsFiles appDirectory
  printIfVerbose outputMode "Found fonts files at: "
  mapM_ (printIfVerbose outputMode) fontFiles
  listOfFontLists <- mapM parseFontsFromFile fontFiles
  let fonts = concat $ rights listOfFontLists
  printIfVerbose outputMode ("Found " ++ (show $ length fonts) ++ (" fonts"))
  let fontHeaderFileStructure = objcHeaderFromFonts fontCategoryName fonts
  let fontMFileStructure = objcImplementationFromFonts fontCategoryName fonts
  printIfVerbose outputMode "Printing fonts files..."
  let fullHeaderPath = appDirectory ++ fontHeaderFileExtension
  let fullMPath = appDirectory ++ fontImplementationFileExtension
  printStructureToFile fontHeaderFileStructure fullHeaderPath
  printStructureToFile fontMFileStructure fullMPath
  printIfVerbose outputMode "Printed fonts to files:"
  printIfVerbose outputMode (fullHeaderPath ++ ", " ++ fullMPath)
  printIfNotSilent outputMode "Finished generating fonts!"

fontCategoryName :: String
fontCategoryName = "MyAppFonts"

fontHeaderFileExtension :: FilePath
fontHeaderFileExtension = "/UIFont+MyAppFonts.h"

fontImplementationFileExtension :: FilePath
fontImplementationFileExtension = "/UIFont+MyAppFonts.m"

---------------------------------------------------------------------------
------------------------PRODUCING ALERTS FILES-----------------------------
---------------------------------------------------------------------------

produceAlertsFiles :: OutputMode -> FilePath -> IO ()
produceAlertsFiles outputMode appDirectory = do
  printIfNotSilent outputMode "Generating alerts..."
  printIfVerbose outputMode "Searching for alerts files..."
  alertFiles <- findAlertsFiles appDirectory
  printIfVerbose outputMode "Found alerts files at: "
  mapM_ (printIfVerbose outputMode) alertFiles
  listOfAlertLists <- mapM parseAlertsFromFile alertFiles
  let alerts = concat $ rights listOfAlertLists
  printIfVerbose outputMode ("Found " ++ (show $ length alerts) ++ (" alerts"))
  let alertHeaderFileStructure = objcHeaderFromAlerts alertCategoryName alerts
  let alertMFileStructure = objcImplementationFromAlerts alertCategoryName alerts
  printIfVerbose outputMode "Printing alerts files..."
  let fullHeaderPath = appDirectory ++ alertHeaderFileExtension
  let fullMPath = appDirectory ++ alertImplementationFileExtension
  printStructureToFile alertHeaderFileStructure fullHeaderPath
  printStructureToFile alertMFileStructure fullMPath
  printIfVerbose outputMode "Printed alerts to files:"
  printIfVerbose outputMode (fullHeaderPath ++ ", " ++ fullMPath)
  printIfNotSilent outputMode "Finished generating alerts!"

alertCategoryName :: String
alertCategoryName = "MyAppAlerts"

alertHeaderFileExtension :: FilePath
alertHeaderFileExtension = "/UIAlertController+MyAppAlerts.h"

alertImplementationFileExtension :: FilePath
alertImplementationFileExtension = "/UIAlertController+MyAppAlerts.m"

---------------------------------------------------------------------------
------------------------PRODUCING ERRORS FILES-----------------------------
---------------------------------------------------------------------------

produceErrorsFiles :: OutputMode -> FilePath -> IO ()
produceErrorsFiles outputMode appDirectory = do
  printIfNotSilent outputMode "Generating errors..."
  printIfVerbose outputMode "Searching for errors files..."
  errorFiles <- findErrorsFiles appDirectory
  printIfVerbose outputMode "Found errors files at: "
  mapM_ (printIfVerbose outputMode) errorFiles
  listOfErrorLists <- mapM parseErrorsFromFile errorFiles
  let errors = concat $ rights listOfErrorLists
  printIfVerbose outputMode ("Found " ++ (show $ length errors) ++ (" errors"))
  let errorHeaderFileStructure = objcHeaderFromErrors errorCategoryName errors
  let errorMFileStructure = objcImplementationFromErrors errorCategoryName errors
  printIfVerbose outputMode "Printing errors files..."
  let fullHeaderPath = appDirectory ++ errorHeaderFileExtension
  let fullMPath = appDirectory ++ errorImplementationFileExtension
  printStructureToFile errorHeaderFileStructure fullHeaderPath
  printStructureToFile errorMFileStructure fullMPath
  printIfVerbose outputMode "Printed errors to files:"
  printIfVerbose outputMode (fullHeaderPath ++ ", " ++ fullMPath)
  printIfNotSilent outputMode "Finished generating errors!"

errorCategoryName :: String
errorCategoryName = "MyAppErrors"

errorHeaderFileExtension :: FilePath
errorHeaderFileExtension = "/NSError+MyAppErrors.h"

errorImplementationFileExtension :: FilePath
errorImplementationFileExtension = "/NSError+MyAppErrors.m"
