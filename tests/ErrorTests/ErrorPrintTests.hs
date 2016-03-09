-- OWAErrorObjc will expose the methods
-- objcHeaderFromErrors :: String -> [OWAError] -> ObjcFile
-- objcImplementationFromErrors :: String -> [OWAError] -> ObjcFile
-- which each take a category name and a list of errors and return a
-- file structure of objective C statements
--
-- OWAObjcPrint will expose the methods
-- printStructureToFile :: ObjcFile -> FilePath -> IO ()
-- which takes an objective C file structure and a filepath
-- and prints the file structure to the given file
--
-- These tests will first create the file structures and then
-- print them, testing the printed files.

module ErrorPrintTests (
  runErrorPrintTests
) where

import OWAErrorObjc
import OWAObjcAbSyn
import OWAObjcPrint
import TestErrors
import TestUtil
import Test.Hspec

runErrorPrintTests :: FilePath -> IO ()
runErrorPrintTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/tests/ErrorTests/ErrorOutputFiles/"
  print testDirectory
