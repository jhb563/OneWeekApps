module Main where

import System.Directory

import AppInfoCLITests
import CodeTypeTests
import LazyCodeGenerationTests
import XCodeTests

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runLazyCodeGenerationTests currentDirectory
  runCodeTypeTests currentDirectory
  runAppInfoCLITests currentDirectory
  runXCodeTests currentDirectory
