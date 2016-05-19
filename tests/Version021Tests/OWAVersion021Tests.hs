module Main where

import ContainerViewTests
import CustomViewTests
import System.Directory

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runCustomViewTests currentDirectory
  runContainerViewTests currentDirectory
