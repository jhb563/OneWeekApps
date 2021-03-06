-- Objc.ColorConverter will expose the methods
-- objcHeaderFromColors :: OWAAppInfo -> [OWAColor] -> ObjcFile
-- objcImplementationFromAlerts :: OWAAppInfo -> [OWAColor] -> ObjcFile
-- which each take an appInfo object and a list of colors and return a
-- file structure of objective C statements
--
-- Objc.Print will expose the method
-- printStructureToFile :: ObjcFile -> FilePath -> IO ()
-- which takes an objective C file structure and a filepath
-- and prints the file structure to the given file
--
-- These tests will first create the file structures and then
-- print them, testing the printed files.

module Objc.Tests.Colors.Basic (
  runColorPrintTests
) where

import Test.Hspec

import Model.OWAAppInfo
import Objc.AbSyn
import Objc.ColorConverter
import Objc.Tests.Colors.Objects
import Objc.Tests.Utils

runColorPrintTests :: FilePath -> IO ()
runColorPrintTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/test/Objc/Tests/Colors/OutputFiles/"
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
fullCategoryTests testDirectory = describe "Print File Structure for Normal Color Category" $ do
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
testFileStructures = [objcHeaderFromColors sampleAppInfo [],
  objcImplementationFromColors sampleAppInfo [],
  objcHeaderFromColors sampleAppInfo testColorsToPrint,
  objcImplementationFromColors sampleAppInfo testColorsToPrint]

resultsFiles :: [String]
resultsFiles = [emptyHeaderResultFile,
  emptyImplementationResultFile,
  headerResultFile,
  implementationResultFile]

emptyHeaderResultFile :: String
emptyHeaderResultFile = "UIColor+EmptyCategory.h"

emptyImplementationResultFile :: String
emptyImplementationResultFile = "UIColor+EmptyCategory.m"

headerResultFile :: String
headerResultFile = "UIColor+MyAppColors.h"

implementationResultFile :: String
implementationResultFile = "UIColor+MyAppColors.m"

emptyHeaderTestFile :: String
emptyHeaderTestFile = "UIColor+EmptyCategory.h.test"

emptyImplementationTestFile :: String
emptyImplementationTestFile = "UIColor+EmptyCategory.m.test"

headerTestFile :: String
headerTestFile = "UIColor+MyAppColors.h.test"

implementationTestFile :: String
implementationTestFile = "UIColor+MyAppColors.m.test"
