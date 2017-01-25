module Main where

import System.Directory

import ContainerViewTests
import CustomViewTests
import Version021IntegrationTests

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runCustomViewTests currentDirectory
  runContainerViewTests currentDirectory
  runV021IntegrationTests currentDirectory
