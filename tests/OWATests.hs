module Main where

import System.Directory
import AppDirectoryTests
import FileSearchTests

main :: IO ()
main = do
  currentFilePath <- getCurrentDirectory
  _ <- runAppDirectoryTests currentFilePath
  runFileSearchTests currentFilePath
