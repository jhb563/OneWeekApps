module Main where

import LazyCodeGenerationTests
import SwiftAlertPrintTests
import SwiftColorPrintTests
import SwiftFontPrintTests
import SwiftViewPrintTests
import System.Directory

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runLazyCodeGenerationTests currentDirectory
  runSwiftColorPrintTests currentDirectory
  runSwiftFontPrintTests currentDirectory
  runSwiftAlertPrintTests currentDirectory
  runSwiftViewPrintTests currentDirectory
