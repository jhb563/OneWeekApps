module Main where

import System.Directory

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
  runErrorParseFailureTests currentDirectory
  runStringsSearchTests currentDirectory
  runStringsPrintTests currentDirectory
  runV015IntegrationTests currentDirectory
