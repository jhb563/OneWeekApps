module Main where

import AlertParseFailureTests
import ColorParseFailureTests
import CommentTests
import DefaultsFileTests
import ErrorParseFailureTests
import FontParseFailureTests
import ItemFailureTests
import SpacingIndentTests
import StringsParseTests
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
  runErrorParseFailureTests currentDirectory
  runSpacingIndentTests currentDirectory
  runDefaultsFileTests currentDirectory
  runStringsParseTests currentDirectory
