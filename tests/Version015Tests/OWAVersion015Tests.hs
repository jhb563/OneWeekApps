module Main where

import System.Directory

import AlertParseFailureTests
import ColorParseFailureTests
import CommentTests
import DefaultsFileTests
import ErrorParseFailureTests
import FontParseFailureTests
import ItemFailureTests
import SpacingIndentTests
import StringsSearchTests
import StringsParseTests
import StringsPrintTests
import TabTests
import Version015IntegrationTests

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
  runStringsSearchTests currentDirectory
  runStringsPrintTests currentDirectory
  runV015IntegrationTests currentDirectory
