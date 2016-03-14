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
runOWA :: FilePath -> IO ()
runOWA filePath = do
  maybeAppDirectory <- findAppDirectory filePath
  case maybeAppDirectory of 
    Nothing -> print "Could not find app directory!"
    Just appDirectory -> do
      produceColorsFiles appDirectory
      produceFontsFiles appDirectory
      produceAlertsFiles appDirectory
      produceErrorsFiles appDirectory

---------------------------------------------------------------------------
------------------------PRODUCING COLORS FILES-----------------------------
---------------------------------------------------------------------------

produceColorsFiles :: FilePath -> IO ()
produceColorsFiles appDirectory = do
  colorFiles <- findColorsFiles appDirectory
  listOfColorLists <- mapM parseColorsFromFile colorFiles
  let colors = concat $ rights listOfColorLists
  let colorHeaderFileStructure = objcHeaderFromColors colorCategoryName colors
  let colorMFileStructure = objcImplementationFromColors colorCategoryName colors
  printStructureToFile colorHeaderFileStructure (appDirectory ++ colorHeaderFileExtension)
  printStructureToFile colorMFileStructure (appDirectory ++ colorImplementationFileExtension)

colorCategoryName :: String
colorCategoryName = "MyAppColors"

colorHeaderFileExtension :: FilePath
colorHeaderFileExtension = "/UIColor+MyAppColors.h"

colorImplementationFileExtension :: FilePath
colorImplementationFileExtension = "/UIColor+MyAppColors.m"

---------------------------------------------------------------------------
------------------------PRODUCING FONTS FILES------------------------------
---------------------------------------------------------------------------

produceFontsFiles :: FilePath -> IO ()
produceFontsFiles appDirectory = do
  fontFiles <- findFontsFiles appDirectory
  listOfFontLists <- mapM parseFontsFromFile fontFiles
  let fonts = concat $ rights listOfFontLists
  let fontHeaderFileStructure = objcHeaderFromFonts fontCategoryName fonts
  let fontMFileStructure = objcImplementationFromFonts fontCategoryName fonts
  printStructureToFile fontHeaderFileStructure (appDirectory ++ fontHeaderFileExtension)
  printStructureToFile fontMFileStructure (appDirectory ++ fontImplementationFileExtension)

fontCategoryName :: String
fontCategoryName = "MyAppFonts"

fontHeaderFileExtension :: FilePath
fontHeaderFileExtension = "/UIFont+MyAppFonts.h"

fontImplementationFileExtension :: FilePath
fontImplementationFileExtension = "/UIFont+MyAppFonts.m"

---------------------------------------------------------------------------
------------------------PRODUCING ALERTS FILES-----------------------------
---------------------------------------------------------------------------

produceAlertsFiles :: FilePath -> IO ()
produceAlertsFiles appDirectory = do
  alertFiles <- findAlertsFiles appDirectory
  listOfAlertLists <- mapM parseAlertsFromFile alertFiles
  let alerts = concat $ rights listOfAlertLists
  let alertHeaderFileStructure = objcHeaderFromAlerts alertCategoryName alerts
  let alertMFileStructure = objcImplementationFromAlerts alertCategoryName alerts
  printStructureToFile alertHeaderFileStructure (appDirectory ++ alertHeaderFileExtension)
  printStructureToFile alertMFileStructure (appDirectory ++ alertImplmentationFileExtension)

alertCategoryName :: String
alertCategoryName = "MyAppAlerts"

alertHeaderFileExtension :: FilePath
alertHeaderFileExtension = "/UIAlertController+MyAppAlerts.h"

alertImplmentationFileExtension :: FilePath
alertImplmentationFileExtension = "/UIAlertController+MyAppAlerts.m"

---------------------------------------------------------------------------
------------------------PRODUCING ERRORS FILES-----------------------------
---------------------------------------------------------------------------

produceErrorsFiles :: FilePath -> IO ()
produceErrorsFiles appDirectory = do
  errorFiles <- findErrorsFiles appDirectory
  listOfErrorLists <- mapM parseErrorsFromFile errorFiles
  let errors = concat listOfErrorLists
  let errorHeaderFileStructure = objcHeaderFromErrors errorCategoryName errors
  let errorMFileStructure = objcImplementationFromErrors errorCategoryName errors
  printStructureToFile errorHeaderFileStructure (appDirectory ++ errorHeaderFileExtension)
  printStructureToFile errorMFileStructure (appDirectory ++ errorImplmentationFileExtension)

errorCategoryName :: String
errorCategoryName = "MyAppErrors"

errorHeaderFileExtension :: FilePath
errorHeaderFileExtension = "/NSError+MyAppErrors.h"

errorImplmentationFileExtension :: FilePath
errorImplmentationFileExtension = "/NSError+MyAppErrors.m"
