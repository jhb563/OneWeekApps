module Main where

import System.Directory
import AppDirectoryTests
import FileSearchTests
import ColorParseTests

main :: IO ()
main = do
  currentFilePath <- getCurrentDirectory
  _ <- runAppDirectoryTests currentFilePath
  _ <- runFileSearchTests currentFilePath
  runColorParseTests currentFilePath
 