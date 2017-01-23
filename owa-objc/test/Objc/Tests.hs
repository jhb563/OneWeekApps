module Main where

import System.Directory (getCurrentDirectory)

import Objc.Tests.Alerts.Basic (runAlertPrintTests)

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runAlertPrintTests currentDirectory
