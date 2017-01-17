module Main where

import System.Directory

import AlertPrintTests
import AppDirectoryTests
import ColorPrintTests
import ErrorParseTests
import ErrorPrintTests
import FileSearchTests
import FontParseTests
import FontPrintTests
import Version010IntegrationTests

main :: IO ()
main = do
  currentFilePath <- getCurrentDirectory
  runAppDirectoryTests currentFilePath
  runFileSearchTests currentFilePath
  runColorPrintTests currentFilePath
  runFontParseTests currentFilePath
  runFontPrintTests currentFilePath
  runAlertPrintTests currentFilePath
  runErrorParseTests currentFilePath
  runErrorPrintTests currentFilePath
  runV010IntegrationTests currentFilePath
