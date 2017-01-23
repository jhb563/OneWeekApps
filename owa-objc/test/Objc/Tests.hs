module Main where

import System.Directory (getCurrentDirectory)

import Objc.Tests.Alerts.Basic (runAlertPrintTests)
import Objc.Tests.Colors.Basic (runColorPrintTests)
import Objc.Tests.Errors.Basic (runErrorPrintTests)
import Objc.Tests.Fonts.Basic (runFontPrintTests)

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runAlertPrintTests currentDirectory
  runColorPrintTests currentDirectory
  runErrorPrintTests currentDirectory
  runFontPrintTests currentDirectory
