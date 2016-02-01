{-|
Module      : OWAFileSearch
Description : Module for searching file tree for relevant files to OneWeekApps
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}
module OWAFileSearch (
    findAppDirectory
) where

import System.Directory
import Data.List.Split
import Control.Monad

-- |'appString' The directory name we are trying to find
appString :: String
appString = "app"

-- | 'findAppDirectory' takes a filepath and returns the filepath of the first directory it finds
-- whose name is 'app', via Breadth first search
findAppDirectory :: FilePath -> IO (Maybe FilePath)
findAppDirectory currentFilePath = do
                                isDirectory <- doesDirectoryExist currentFilePath
                                if isDirectory then findAppDirectoryHelper [currentFilePath] else return Nothing

-- |'findAppDirectoryHelper' is the tail recursion helper for findAppDirectory. It does the bulk
-- of the work in performing the BFS on the file tree.
findAppDirectoryHelper :: [FilePath] -> IO (Maybe FilePath)
findAppDirectoryHelper [] = return Nothing
findAppDirectoryHelper (fPath:queue) = if isTargetDir fPath then return (Just fPath)
                                        else do
                                            newPaths <- listDirectory fPath
                                            newDirectories <- filterM doesDirectoryExist (map (\path -> fPath ++ ('/':path)) newPaths)
                                            let newQueue = queue ++ newDirectories
                                            findAppDirectoryHelper newQueue

-- |'isTargetDir' Tells us if the given directory ends in our appString.
isTargetDir :: FilePath -> Bool
isTargetDir fPath = last components == appString
                where components = splitOn "/" fPath

