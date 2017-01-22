module Main where

import System.Directory

import Version020IntegrationTests
import ViewFileSearchTests
import ViewItemErrorTests
import ViewParseErrorTests
import ViewPrintTests

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runViewFileSearchTests currentDirectory
  runViewPrintTests currentDirectory
  runViewItemErrorTests currentDirectory
  runViewParseErrorTests currentDirectory
  runV020IntegrationTests currentDirectory
