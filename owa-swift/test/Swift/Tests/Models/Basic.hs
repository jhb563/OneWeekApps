-- Swift.ModelConverter will expose the functions:
-- swiftHeaderFromModel :: OWAAppInfo -> OWAModel -> SwiftFile
-- swiftImplementationFromModel :: OWAAppInfo -> OWAView -> SwiftFile
-- which each take an appInfo object and a view and return a
-- file structure of objective C statements
--
-- Swift.Print will expose the method:
-- printStructureToFile :: SwiftFile -> FilePath -> IO ()
-- which takes an objective C file structure and a filepath
-- and prints the file structure ot the give file
--
-- These tests will first create the file structures and then
-- print them, testing the printed files.

module Swift.Tests.Models.Basic (
  runSwiftModelPrintTests
) where

import Test.Hspec

import Model.OWAAppInfo
import Swift.AbSyn
import Swift.Tests.Utils
import Swift.Tests.Models.Objects
import Swift.ModelConverter

runSwiftModelPrintTests :: FilePath -> IO ()
runSwiftModelPrintTests _ = print "Model Print tests stubbed out!"
