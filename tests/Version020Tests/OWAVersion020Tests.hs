module Main where

import System.Directory
import ViewFileSearchTests
import ViewElementTests
import ViewNameTests

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runViewFileSearchTests currentDirectory
  runViewNameTests currentDirectory
  runViewElementTests currentDirectory
