module Main where

import System.Directory

import Version020IntegrationTests

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runV020IntegrationTests currentDirectory
