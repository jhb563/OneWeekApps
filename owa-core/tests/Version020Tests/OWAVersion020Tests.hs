module Main where

import System.Directory

import Version020IntegrationTests
import ViewFileSearchTests

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runViewFileSearchTests currentDirectory
  runV020IntegrationTests currentDirectory
