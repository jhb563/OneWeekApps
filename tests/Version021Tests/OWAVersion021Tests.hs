module Main where

import System.Directory

import ContainerViewTests
import CustomViewTests
import ImageButtonTests
import Version021IntegrationTests
import ViewErrorTests

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runCustomViewTests currentDirectory
  runContainerViewTests currentDirectory
  runViewErrorTests currentDirectory
  runImageButtonTests currentDirectory
  runV021IntegrationTests currentDirectory
