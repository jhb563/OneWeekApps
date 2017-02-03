module Main where

import System.Directory

import Version010IntegrationTests

main :: IO ()
main = do
  currentFilePath <- getCurrentDirectory
  runV010IntegrationTests currentFilePath
