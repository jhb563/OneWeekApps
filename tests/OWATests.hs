module Main where

import System.Directory
import AppDirectoryTests
import FileSearchTests
import ColorParseTests
import ColorObjcTests
import FontParseTests
import ObjcPrintTests
import Version010IntegrationTests

main :: IO ()
main = do
  currentFilePath <- getCurrentDirectory
  runAppDirectoryTests currentFilePath
  runFileSearchTests currentFilePath
  runColorParseTests currentFilePath
  runColorObjcTests
  runFontParseTests currentFilePath
  runObjcPrintTests currentFilePath
  runV010IntegrationTests currentFilePath
