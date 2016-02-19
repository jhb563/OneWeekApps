module Main where

import System.Directory
import AppDirectoryTests
import FileSearchTests
import ColorParseTests
import ColorObjcTests
import ObjcPrintTests

main :: IO ()
main = do
  currentFilePath <- getCurrentDirectory
  runAppDirectoryTests currentFilePath
  runFileSearchTests currentFilePath
  runColorParseTests currentFilePath
  runColorObjcTests
  runObjcPrintTests currentFilePath
