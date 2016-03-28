-- OWAAlertObjc will expose the methods
-- objcHeaderFromAlerts :: OWAAppInfo -> [OWAAlert] -> ObjcFile
-- objcImplementationFromAlerts :: OWAAppInfo -> [OWAAlert] -> ObjcFile
-- which each take an appInfo object and a list of alerts and return a
-- file structure of objective C statements
--
-- OWAObjcPrint will expose the methods
-- printStructureToFile :: ObjcFile -> FilePath -> IO ()
-- which takes an objective C file structure and a filepath
-- and prints the file structure to the given file
--
-- These tests will first create the file structures and then
-- print them, testing the printed files.

module AlertPrintTests (
  runAlertPrintTests
) where

import OWAAppInfo
import OWAAlertObjc
import OWAObjcAbSyn
import TestAlerts
import TestUtil
import Test.Hspec

runAlertPrintTests :: FilePath -> IO ()
runAlertPrintTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/tests/Version010Tests/AlertTests/AlertOutputFiles/"
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
fullCategoryTests testDirectory = describe "Print File Structure for Normal Alert Category" $ do
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
testFileStructures = [objcHeaderFromAlerts sampleAppInfo [],
  objcImplementationFromAlerts sampleAppInfo [],
  objcHeaderFromAlerts sampleAppInfo allTestAlerts,
  objcImplementationFromAlerts sampleAppInfo allTestAlerts]

resultsFiles :: [String]
resultsFiles = [emptyHeaderResultFile,
  emptyImplementationResultFile,
  headerResultFile,
  implementationResultFile]

emptyHeaderResultFile :: String
emptyHeaderResultFile = "UIAlertController+EmptyCategory.h"

emptyImplementationResultFile :: String
emptyImplementationResultFile = "UIAlertController+EmptyCategory.m"

headerResultFile :: String
headerResultFile = "UIAlertController+MyAppAlerts.h"

implementationResultFile :: String
implementationResultFile = "UIAlertController+MyAppAlerts.m"

emptyHeaderTestFile :: String
emptyHeaderTestFile = "UIAlertController+EmptyCategory.h.test"

emptyImplementationTestFile :: String
emptyImplementationTestFile = "UIAlertController+EmptyCategory.m.test"

headerTestFile :: String
headerTestFile = "UIAlertController+MyAppAlerts.h.test"

implementationTestFile :: String
implementationTestFile = "UIAlertController+MyAppAlerts.m.test"

