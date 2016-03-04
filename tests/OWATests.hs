module Main where

import AppDirectoryTests
import ColorObjcTests
import ColorParseTests
import FileSearchTests
import FontObjcTests
import FontParseTests
import FontPrintTests
import ObjcPrintTests
import System.Directory
import Version010IntegrationTests

main :: IO ()
main = do
  currentFilePath <- getCurrentDirectory
  runAppDirectoryTests currentFilePath
  runFileSearchTests currentFilePath
  runColorParseTests currentFilePath
  runColorObjcTests
  runFontParseTests currentFilePath
  runFontObjcTests
  runFontPrintTests currentFilePath
  runObjcPrintTests currentFilePath
  runV010IntegrationTests currentFilePath
