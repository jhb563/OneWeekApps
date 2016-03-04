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

import OWAColor
import OWAColorObjc
import OWAColorParser
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

---------------------------------------------------------------------------
------------------------PRODUCING COLORS FILES-----------------------------
---------------------------------------------------------------------------

produceColorsFiles :: FilePath -> IO ()
produceColorsFiles appDirectory = do
  colorFiles <- findColorsFiles appDirectory
  listOfColorLists <- mapM parseColorsFromFile colorFiles
  let colors = concat listOfColorLists
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
------------------------PRODUCING COLORS FILES-----------------------------
---------------------------------------------------------------------------

produceFontsFiles :: FilePath -> IO ()
produceFontsFiles appDirectory = do
  fontFiles <- findFontsFiles appDirectory
  listOfFontLists <- mapM parseFontsFromFile fontFiles
  let fonts = concat listOfFontLists
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
