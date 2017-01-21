module Main where

import System.Directory

import FontParseFailureTests
import ItemFailureTests
import StringsSearchTests
import StringsPrintTests
import Version015IntegrationTests

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runItemFailureTests currentDirectory
  runFontParseFailureTests currentDirectory
  runStringsSearchTests currentDirectory
  runStringsPrintTests currentDirectory
  runV015IntegrationTests currentDirectory
