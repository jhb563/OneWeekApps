module Main where

import System.Directory

import Version021IntegrationTests

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runV021IntegrationTests currentDirectory
