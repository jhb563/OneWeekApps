module Main where

import AppInfoCLITests
import CodeTypeTests
import LazyCodeGenerationTests
import SwiftAlertPrintTests
import SwiftColorPrintTests
import SwiftErrorPrintTests
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
  runSwiftErrorPrintTests currentDirectory
  runSwiftViewPrintTests currentDirectory
  runCodeTypeTests currentDirectory
  runAppInfoCLITests currentDirectory
