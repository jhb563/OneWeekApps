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

import OWAColorObjc
import OWAColorParser
import OWAFileSearch
import OWAObjcPrint

runOWA :: FilePath -> IO ()
runOWA filePath = do
  maybeAppDirectory <- findAppDirectory filePath
  case maybeAppDirectory of 
    Nothing -> print "Could not find app directory!"
    Just appDirectory -> do
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
