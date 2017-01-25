module Main where

import System.Directory

import StringsSearchTests
import Version015IntegrationTests

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runStringsSearchTests currentDirectory
  runV015IntegrationTests currentDirectory
