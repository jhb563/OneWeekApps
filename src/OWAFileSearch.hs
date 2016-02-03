{-|
Module      : OWAFileSearch
Description : Module for searching file tree for relevant files to OneWeekApps
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}
module OWAFileSearch (
    findAppDirectory,
    findColorsFiles,
    findFontsFiles,
    findAlertsFiles,
    findErrorsFiles
) where

import System.Directory
import Data.List.Split
import Control.Monad

--------------------------------------------------------------------------------------------------------------------
-----------------------------------Finding App Directory -----------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------

-- | 'appString' The directory name we are trying to find
appString :: String
appString = "app"

-- | 'findAppDirectory' takes a filepath and returns the filepath of the first directory it finds
-- whose name is 'app', via Breadth first search
findAppDirectory :: FilePath -> IO (Maybe FilePath)
findAppDirectory currentFilePath = do
  isDirectory <- doesDirectoryExist currentFilePath
  if isDirectory then findAppDirectoryHelper [currentFilePath] else return Nothing

-- | 'findAppDirectoryHelper' is the tail recursion helper for findAppDirectory. It does the bulk
-- of the work in performing the BFS on the file tree.
findAppDirectoryHelper :: [FilePath] -> IO (Maybe FilePath)
findAppDirectoryHelper [] = return Nothing
findAppDirectoryHelper (fPath:queue) = if isTargetDir fPath 
  then return (Just fPath)
  else do
    newPaths <- listDirectory fPath
    newDirectories <- filterM doesDirectoryExist (map (\path -> fPath ++ ('/':path)) newPaths)
    let newQueue = queue ++ newDirectories
    findAppDirectoryHelper newQueue

-- | 'isTargetDir' Tells us if the given directory ends in our appString.
isTargetDir :: FilePath -> Bool
isTargetDir fPath = last components == appString
  where components = splitOn "/" fPath

--------------------------------------------------------------------------------------------------------------------
-----------------------------------Finding Input Files -------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------

-- | 'findColorsFiles' Locates all the files with the extension '.colors', searching recursively
-- from the given directory.
findColorsFiles :: FilePath -> IO [FilePath]
findColorsFiles appDirectory = searchDirectoryForExtension colorsExtension [appDirectory] []

-- | 'findFontsFiles' Locates all the files with the extension '.fonts', searching recursively
-- from the given directory.
findFontsFiles :: FilePath -> IO [FilePath]
findFontsFiles appDirectory = searchDirectoryForExtension fontsExtension [appDirectory] []

-- | 'findAlertsFiles' Locates all the files with the extension '.alerts', searching recursively
-- from the given directory.
findAlertsFiles :: FilePath -> IO [FilePath]
findAlertsFiles appDirectory = searchDirectoryForExtension alertsExtension [appDirectory] []

-- | 'findErrorsFiles' Locates all the files with the extension '.errors', searching recursively
-- from the given directory.
findErrorsFiles :: FilePath -> IO [FilePath]
findErrorsFiles appDirectory = searchDirectoryForExtension errorsExtension [appDirectory] []

colorsExtension :: String
colorsExtension = "colors"

fontsExtension :: String
fontsExtension = "fonts"

alertsExtension :: String
alertsExtension = "alerts"

errorsExtension :: String
errorsExtension = "errors"

-- | 'searchDirectoryForExtension' is a common helper method doing most of the work
-- for searching for the files. It uses BFS on the directory. It includes a queue of unexplored
-- directories, as well as an accumulator argument for the files found.
searchDirectoryForExtension :: String -> [FilePath] -> [FilePath] -> IO [FilePath]
searchDirectoryForExtension extension [] locatedFiles = return locatedFiles
searchDirectoryForExtension extension (nextFilePath:queue) locatedFiles = do
  directoryListing <- listDirectory nextFilePath
  let absolutePaths = map (\fp -> nextFilePath ++ ('/':fp)) directoryListing
  filesInDirectory <- filterM doesFileExist absolutePaths
  subdirectories <- filterM doesDirectoryExist absolutePaths
  let matchingFiles = filter (fileHasTargetExtension extension) filesInDirectory
  let newLocatedFiles = matchingFiles ++ locatedFiles
  let newQueue = queue ++ subdirectories
  searchDirectoryForExtension extension newQueue newLocatedFiles

-- | 'fileHasTargetExtension' tells us if the given file ends with the given extension
fileHasTargetExtension :: String -> FilePath -> Bool
fileHasTargetExtension extension filePath = last components == extension
  where components = splitOn "." filePath
