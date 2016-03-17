module Main where

import AlertParseFailureTests
import ColorParseFailureTests
import CommentTests
import FontParseFailureTests
import ItemFailureTests
import TabTests
import System.Directory

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runCommentTests currentDirectory
  runTabTests currentDirectory
  runItemFailureTests currentDirectory
  runColorParseFailureTests currentDirectory
  runFontParseFailureTests currentDirectory
  runAlertParseFailureTests currentDirectory
