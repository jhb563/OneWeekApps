module Main where

import System.Directory
import ViewConstraintTests
import ViewElementTests
import ViewFileSearchTests
import ViewItemErrorTests
import ViewNameTests
import ViewPrintTests

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runViewFileSearchTests currentDirectory
  runViewNameTests currentDirectory
  runViewElementTests currentDirectory
  runViewConstraintTests currentDirectory
  runViewPrintTests currentDirectory
  runViewItemErrorTests currentDirectory
