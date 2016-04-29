module Main where

import System.Directory
import ViewConstraintTests
import ViewElementTests
import ViewFileSearchTests
import ViewNameTests

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runViewFileSearchTests currentDirectory
  runViewNameTests currentDirectory
  runViewElementTests currentDirectory
  runViewConstraintTests currentDirectory
