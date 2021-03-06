-- Swift.AlertConverter will expose the method:
-- swiftExtensionFromAlerts :: OWAAppInfo -> [OWAAlert] -> SwiftFile
-- which take an appInfo object and a list of alerts and return a
-- file structure of Swift statements
--
-- Swift.Print will expose the method
-- printStructureToFile :: SwiftFile -> FilePath -> IO ()
-- which takes a Swift file structure and a filepath and
-- prints the file structure to the given file
--
-- These tests will first create the file structures and then
-- print them, testing the printed files.

module Swift.Tests.Alerts.Basic (
  runSwiftAlertPrintTests
) where

import Test.Hspec

import Model.OWAAppInfo
import Swift.AbSyn
import Swift.AlertConverter
import Swift.Tests.Alerts.Objects
import Swift.Tests.Utils

runSwiftAlertPrintTests :: FilePath -> IO ()
runSwiftAlertPrintTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/test/Swift/Tests/Alerts/OutputFiles/"
  hspec $
    beforeAll_ (removeDiffFiles testDirectory) $
    beforeAll_ (createSwiftResultsFiles testDirectory resultsFiles testFileStructures)
    . afterAll_ (removeResultsFiles testDirectory resultsFiles) $ do
      emptyExtensionTests testDirectory
      fullExtensionTests testDirectory

emptyExtensionTests :: FilePath -> Spec
emptyExtensionTests testDirectory = describe "Print File Structure for Empty Extension" $
  it "The printed file should match" $
    (testDirectory ++ emptyResultFile) `filesShouldMatch`
      (testDirectory ++ emptyTestFile)

fullExtensionTests :: FilePath -> Spec
fullExtensionTests testDirectory = describe "Print File Structure for Full Extension" $
  it "The printed file should match" $
    (testDirectory ++ fullResultFile) `filesShouldMatch`
      (testDirectory ++ fullTestFile)

sampleAppInfo :: OWAAppInfo
sampleAppInfo = OWAAppInfo {
  appName = "MySampleApp",
  appPrefix = "MSA",
  authorName = "James Bowen",
  dateCreatedString = "2/16/2016",
  companyName = Just "One Week Apps"
}

testFileStructures :: [SwiftFile]
testFileStructures = [swiftExtensionFromAlerts sampleAppInfo [],
  swiftExtensionFromAlerts sampleAppInfo swiftTestAlerts]

resultsFiles :: [String]
resultsFiles = [emptyResultFile,
  fullResultFile]

emptyResultFile :: String
emptyResultFile = "UIAlertController+EmptyCategory.swift"

fullResultFile :: String
fullResultFile = "UIAlertController+MSAAlerts.swift"

emptyTestFile :: String
emptyTestFile = "UIAlertController+EmptyCategory.swift.test"

fullTestFile :: String
fullTestFile = "UIAlertController+MSAAlerts.swift.test"
