-- OWAColorObjc will expose the methods
-- objcHeaderFromColors :: String -> [OWAColor] -> ObjcFile
-- objcImplementationFromAlerts :: String -> [OWAColor] -> ObjcFile
-- which each take a category name and a list of colors and return a
-- file structure of objective C statements
--
-- OWAObjcPrint will expose the method
-- printStructureToFile :: ObjcFile -> FilePath -> IO ()
-- which takes an objective C file structure and a filepath
-- and prints the file structure to the given file
--
-- These tests will first create the file structures and then
-- print them, testing the printed files.

module ObjcPrintTests (
  runObjcPrintTests
) where

import ColorTestUtil
import OWAColor
import OWAObjcAbSyn
import OWAObjcPrint
import System.Directory
import TestUtil
import Test.Hspec

runObjcPrintTests :: FilePath -> IO ()
runObjcPrintTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/tests/ColorTests/ColorOutputFiles/"
  hspec $
    beforeAll_ (removeDiffFiles testDirectory) $
    beforeAll_ (createResultsFiles testDirectory resultsFiles testFileStructures)
    . afterAll_ (removeResultsFiles testDirectory resultsFiles) $ do
      emptyCategoryTests
      fullCategoryTests

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

testFileStructures :: [ObjcFile]
testFileStructures = [objcHeaderFromColors "EmptyCategory" [],
  objcImplementationFromColors "EmptyColors" [],
  objcHeaderFromColors "MyAppColors" testColorsToPrint,
  objcImplementationFromColors "MyAppColors" testColorsToPrint]

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