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
findAppDirectory currentFilePath = do
                                isDirectory <- doesDirectoryExist currentFilePath
                                if isDirectory then findAppDirectoryHelper [currentFilePath] else return Nothing

findAppDirectoryHelper :: [FilePath] -> IO (Maybe FilePath)
findAppDirectoryHelper [] = return Nothing
findAppDirectoryHelper (fPath:queue) = if isTargetDir fPath then return (Just fPath)
                                        else do
                                            newPaths <- listDirectory fPath
                                            newDirectories <- filterM doesDirectoryExist (map (\path -> fPath ++ ('/':path)) newPaths)
                                            let newQueue = queue ++ newDirectories
                                            findAppDirectoryHelper newQueue

-- | 'isTargetDir' Tells us if the given directory ends in our appString.
isTargetDir :: FilePath -> Bool
isTargetDir fPath = last components == appString
                where components = splitOn "/" fPath

