module Main where

import ContainerViewTests
import CustomViewTests
import ImageButtonTests
import Version021IntegrationTests
import ViewErrorTests
import System.Directory

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runCustomViewTests currentDirectory
  runContainerViewTests currentDirectory
  runViewErrorTests currentDirectory
  runImageButtonTests currentDirectory
  runV021IntegrationTests currentDirectory
