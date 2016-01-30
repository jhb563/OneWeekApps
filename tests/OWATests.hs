module Main where

import System.Directory
import AppDirectoryTests

main :: IO ()
main = do
    currentFilePath <- getCurrentDirectory
    runAppDirectoryTests currentFilePath
