module Main where

import LazyCodeGenerationTests
import SwiftColorPrintTests
import SwiftFontPrintTests
import System.Directory

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runLazyCodeGenerationTests currentDirectory
  runSwiftColorPrintTests currentDirectory
  runSwiftFontPrintTests currentDirectory
