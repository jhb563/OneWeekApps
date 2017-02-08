module Main where

import System.Directory (getCurrentDirectory)

import Swift.Tests.Alerts.Basic (runSwiftAlertPrintTests)
import Swift.Tests.Colors.Basic (runSwiftColorPrintTests)
import Swift.Tests.Errors.Basic (runSwiftErrorPrintTests)
import Swift.Tests.Fonts.Basic (runSwiftFontPrintTests)
import Swift.Tests.Views.Basic (runSwiftViewPrintTests)

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runSwiftAlertPrintTests currentDirectory 
  runSwiftColorPrintTests currentDirectory 
  runSwiftErrorPrintTests currentDirectory 
  runSwiftFontPrintTests currentDirectory 
  runSwiftViewPrintTests currentDirectory 
