module Main where

import AlertParseTests
import AlertPrintTests
import AppDirectoryTests
import ColorObjcTests
import ColorParseTests
import ErrorParseTests
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
  runAlertParseTests currentFilePath
  runAlertPrintTests currentFilePath
  runErrorParseTests currentFilePath
  runObjcPrintTests currentFilePath
  runV010IntegrationTests currentFilePath
