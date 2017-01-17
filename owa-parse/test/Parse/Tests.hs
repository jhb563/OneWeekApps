module Main where

import System.Directory (getCurrentDirectory)

import Parse.Tests.Alerts.Basic (runAlertParseTests)
import Parse.Tests.Colors.Basic (runColorParseTests)

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory 
  runAlertParseTests currentDirectory
  runColorParseTests currentDirectory
