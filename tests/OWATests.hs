module Main where

import System.Directory
import AppDirectoryTests
import FileSearchTests
import ColorParseTests
import ColorObjcTests
import ObjcPrintTests
import Version010IntegrationTests

main :: IO ()
main = do
  currentFilePath <- getCurrentDirectory
  runAppDirectoryTests currentFilePath
  runFileSearchTests currentFilePath
  runColorParseTests currentFilePath
  runColorObjcTests
  runObjcPrintTests currentFilePath
  runV010IntegrationTests currentFilePath
