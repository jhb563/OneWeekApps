module Main where

import System.Directory

import AlertPrintTests
import AppDirectoryTests
import ColorPrintTests
import ErrorPrintTests
import FileSearchTests
import FontPrintTests
import Version010IntegrationTests

main :: IO ()
main = do
  currentFilePath <- getCurrentDirectory
  runAppDirectoryTests currentFilePath
  runFileSearchTests currentFilePath
  runColorPrintTests currentFilePath
  runFontPrintTests currentFilePath
  runAlertPrintTests currentFilePath
  runErrorPrintTests currentFilePath
  runV010IntegrationTests currentFilePath
