module Main where

import System.Directory
import AppDirectoryTests
import FileSearchTests
import ColorParseTests
import ColorObjcTests

main :: IO ()
main = do
  currentFilePath <- getCurrentDirectory
  runAppDirectoryTests currentFilePath
  runFileSearchTests currentFilePath
  runColorParseTests currentFilePath
  runColorObjcTests
