module Main where

import System.Directory
import AppDirectoryTests
import FileSearchTests
import ColorParseTests

main :: IO ()
main = do
  currentFilePath <- getCurrentDirectory
  runAppDirectoryTests currentFilePath
  runFileSearchTests currentFilePath
  runColorParseTests currentFilePath
 