module Main where

import AlertParseTests
import AlertPrintTests
import AppDirectoryTests
import ColorParseTests
import ErrorParseTests
import ErrorPrintTests
import FileSearchTests
import FontParseTests
import FontPrintTests
import System.Directory
import Version010IntegrationTests

main :: IO ()
main = do
  currentFilePath <- getCurrentDirectory
  runAppDirectoryTests currentFilePath
  runFileSearchTests currentFilePath
  runColorParseTests currentFilePath
  runFontParseTests currentFilePath
  runFontPrintTests currentFilePath
  runAlertParseTests currentFilePath
  runAlertPrintTests currentFilePath
  runErrorParseTests currentFilePath
  runErrorPrintTests currentFilePath
  runV010IntegrationTests currentFilePath
