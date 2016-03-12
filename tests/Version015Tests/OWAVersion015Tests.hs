module Main where

import CommentTests
import TabTests
import System.Directory

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runCommentTests currentDirectory
  runTabTests currentDirectory
