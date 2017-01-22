module Main where

import System.Directory

import Version020IntegrationTests
import ViewFileSearchTests
import ViewPrintTests

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runViewFileSearchTests currentDirectory
  runViewPrintTests currentDirectory
  runV020IntegrationTests currentDirectory
