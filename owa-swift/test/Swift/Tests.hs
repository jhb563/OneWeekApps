module Main where

import System.Directory (getCurrentDirectory)

import Swift.Tests.Alerts.Basic (runSwiftAlertPrintTests)

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runSwiftAlertPrintTests currentDirectory 
