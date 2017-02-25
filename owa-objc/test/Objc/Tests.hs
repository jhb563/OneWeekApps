module Main where

import System.Directory (getCurrentDirectory)

import Objc.Tests.Alerts.Basic (runAlertPrintTests)
import Objc.Tests.Colors.Basic (runColorPrintTests)
import Objc.Tests.Errors.Basic (runErrorPrintTests)
import Objc.Tests.Fonts.Basic (runFontPrintTests)
import Objc.Tests.Models.Basic (runModelPrintTests)
import Objc.Tests.Strings.Basic (runStringsPrintTests)
import Objc.Tests.Views.Basic (runViewPrintTests)
import Objc.Tests.Views.Containers (runContainerViewPrintTests)
import Objc.Tests.Views.Custom (runCustomViewPrintTests)

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runAlertPrintTests currentDirectory
  runColorPrintTests currentDirectory
  runErrorPrintTests currentDirectory
  runFontPrintTests currentDirectory
  runViewPrintTests currentDirectory
  runContainerViewPrintTests currentDirectory
  runCustomViewPrintTests currentDirectory
  runModelPrintTests currentDirectory
