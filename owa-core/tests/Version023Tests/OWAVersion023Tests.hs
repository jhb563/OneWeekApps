module Main where

import System.Directory

import AppInfoCLITests
import CodeTypeTests
import LazyCodeGenerationTests
import Version023IntegrationTests
import XCodeTests

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runLazyCodeGenerationTests currentDirectory
  runCodeTypeTests currentDirectory
  runAppInfoCLITests currentDirectory
  runXCodeTests currentDirectory
  runV023IntegrationTests currentDirectory
