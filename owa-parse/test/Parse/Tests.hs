module Main where

import System.Directory (getCurrentDirectory)

import Parse.Tests.Alerts.Basic (runAlertParseTests)
import Parse.Tests.Colors.Basic (runColorParseTests)
import Parse.Tests.Errors.Basic (runErrorParseTests)
import Parse.Tests.Fonts.Basic (runFontParseTests)

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory 
  runAlertParseTests currentDirectory
  runColorParseTests currentDirectory
  runErrorParseTests currentDirectory
  runFontParseTests currentDirectory
