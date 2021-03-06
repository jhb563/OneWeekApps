-- Objc.ErrorConverter will expose the methods
-- objcHeaderFromErrors :: OWAAppInfo -> [OWAError] -> ObjcFile
-- objcImplementationFromErrors :: OWAAppInfo -> [OWAError] -> ObjcFile
-- which each take an appInfo object and a list of errors and return a
-- file structure of objective C statements
--
-- Objc.Print will expose the methods
-- printStructureToFile :: ObjcFile -> FilePath -> IO ()
-- which takes an objective C file structure and a filepath
-- and prints the file structure to the given file
--
-- These tests will first create the file structures and then
-- print them, testing the printed files.

module Objc.Tests.Errors.Basic (
  runErrorPrintTests
) where

import Test.Hspec

import Model.OWAAppInfo
import Objc.AbSyn
import Objc.ErrorConverter
import Objc.Tests.Errors.Objects
import Objc.Tests.Utils

runErrorPrintTests :: FilePath -> IO ()
runErrorPrintTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/test/Objc/Tests/Errors/OutputFiles/"
  hspec $
    beforeAll_ (removeDiffFiles testDirectory) $
    beforeAll_ (createResultsFiles testDirectory resultsFiles testFileStructures)
    . afterAll_ (removeResultsFiles testDirectory resultsFiles) $ do
      emptyCategoryTests testDirectory
      fullCategoryTests testDirectory

emptyCategoryTests :: FilePath -> Spec
emptyCategoryTests testDirectory = describe "Print File Structure for Empty Category" $ do
  it "The printed header file should match" $
    (testDirectory ++ emptyHeaderResultFile) `filesShouldMatch` 
      (testDirectory ++ emptyHeaderTestFile)
  
  it "The printed implementation file should match" $
    (testDirectory ++ emptyImplementationResultFile) `filesShouldMatch`
      (testDirectory ++ emptyImplementationTestFile) 

fullCategoryTests :: FilePath -> Spec
fullCategoryTests testDirectory = describe "Print File Structure for Normal Error Category" $ do
  it "The printed header file should match" $
    (testDirectory ++ headerResultFile) `filesShouldMatch`
      (testDirectory ++ headerTestFile) 

  it "The printed implementation file should match" $
    (testDirectory ++ implementationResultFile) `filesShouldMatch`
      (testDirectory ++ implementationTestFile)

sampleAppInfo :: OWAAppInfo
sampleAppInfo = OWAAppInfo {
  appName = "MySampleApp",
  appPrefix = "MSA",
  authorName = "James Bowen",
  dateCreatedString = "2/16/2016",
  companyName = Just "One Week Apps"
}

testFileStructures :: [ObjcFile]
testFileStructures = [objcHeaderFromErrors sampleAppInfo [],
  objcImplementationFromErrors sampleAppInfo [],
  objcHeaderFromErrors sampleAppInfo allTestErrors,
  objcImplementationFromErrors sampleAppInfo allTestErrors]

resultsFiles :: [String]
resultsFiles = [emptyHeaderResultFile,
  emptyImplementationResultFile,
  headerResultFile,
  implementationResultFile]

emptyHeaderResultFile :: String
emptyHeaderResultFile = "NSError+EmptyCategory.h"

emptyImplementationResultFile :: String
emptyImplementationResultFile = "NSError+EmptyCategory.m"

headerResultFile :: String
headerResultFile = "NSError+MyAppErrors.h"

implementationResultFile :: String
implementationResultFile = "NSError+MyAppErrors.m"

emptyHeaderTestFile :: String
emptyHeaderTestFile = "NSError+EmptyCategory.h.test"

emptyImplementationTestFile :: String
emptyImplementationTestFile = "NSError+EmptyCategory.m.test"

headerTestFile :: String
headerTestFile = "NSError+MyAppErrors.h.test"

implementationTestFile :: String
implementationTestFile = "NSError+MyAppErrors.m.test"
