module Main where

import CommentTests
import ItemFailureTests
import TabTests
import System.Directory

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runCommentTests currentDirectory
  runTabTests currentDirectory
  runItemFailureTests currentDirectory
