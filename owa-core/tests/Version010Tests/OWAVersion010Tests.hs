module Main where

import System.Directory

import AppDirectoryTests
import FileSearchTests
import Version010IntegrationTests

main :: IO ()
main = do
  currentFilePath <- getCurrentDirectory
  runAppDirectoryTests currentFilePath
  runFileSearchTests currentFilePath
  runV010IntegrationTests currentFilePath
