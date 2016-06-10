module Main where

import ContainerViewTests
import CustomViewTests
import ImageButtonTests
import ViewErrorTests
import System.Directory

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runCustomViewTests currentDirectory
  runContainerViewTests currentDirectory
  runViewErrorTests currentDirectory
  runImageButtonTests currentDirectory
