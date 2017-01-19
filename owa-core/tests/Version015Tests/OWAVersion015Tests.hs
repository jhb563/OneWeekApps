module Main where

import System.Directory

import AlertParseFailureTests
import ColorParseFailureTests
import ErrorParseFailureTests
import FontParseFailureTests
import ItemFailureTests
import StringsSearchTests
import StringsPrintTests
import Version015IntegrationTests

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runItemFailureTests currentDirectory
  runColorParseFailureTests currentDirectory
  runFontParseFailureTests currentDirectory
  runAlertParseFailureTests currentDirectory
  runErrorParseFailureTests currentDirectory
  runStringsSearchTests currentDirectory
  runStringsPrintTests currentDirectory
  runV015IntegrationTests currentDirectory
