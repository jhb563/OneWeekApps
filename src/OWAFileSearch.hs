module OWAFileSearch (
    findAppDirectory
) where

import System.Directory
import Data.List.Split
import Control.Monad

-- | 'appString' The directory name we are trying to find
appString :: String
appString = "app"

-- | 'findAppDirectory' Given a filepath, returns the filepath of the first directory it finds
-- whose name is 'app', via Breadth first search
findAppDirectory :: FilePath -> IO (Maybe FilePath)
findAppDirectory currentFilePath = findAppDirectoryHelper currentFilePath []

findAppDirectoryHelper :: FilePath -> [FilePath] -> IO (Maybe FilePath)
findAppDirectoryHelper fPath queue = if isTargetDir fPath then return (Just fPath)
                                        else do
                                            newPaths <- listDirectory fPath
                                            newDirectories <- filterM doesDirectoryExist (map (\path -> fPath ++ ('/':path)) newPaths)
                                            let newQueue = queue ++ newDirectories
                                            if length newQueue > 0 then 
                                                findAppDirectoryHelper (head newQueue) (tail newQueue)
                                                else return Nothing

-- | 'isTargetDir' Tells us if the given directory ends in our appString.
isTargetDir :: FilePath -> Bool
isTargetDir fPath = last components == appString
                where components = splitOn "/" fPath

