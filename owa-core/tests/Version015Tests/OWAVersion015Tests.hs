module Main where

import System.Directory

import Version015IntegrationTests

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runV015IntegrationTests currentDirectory
