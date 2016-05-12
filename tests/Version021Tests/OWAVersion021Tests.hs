module Main where

import CustomViewTests
import System.Directory

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runCustomViewTests currentDirectory
