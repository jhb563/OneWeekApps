-- Swift.ColorConverter will expose the method:
-- swiftExtensionFromColors :: OWAAppInfo -> [OWAColor] -> SwiftFile
-- which take an appInfo object and a list of colors and return a
-- file structure of Swift statements
--
-- Swift.Print will expose the method
-- printStructureToFile :: SwiftFile -> FilePath -> IO ()
-- which takes a Swift file structure and a filepath and
-- prints the file structure to the given file
--
-- These tests will first create the file structures and then
-- print them, testing the printed files.

module Swift.Tests.Colors.Basic (
  runSwiftColorPrintTests
) where

import Test.Hspec

import Model.OWAAppInfo
import Swift.AbSyn
import Swift.ColorConverter
import Swift.Tests.Colors.Objects
import Swift.Tests.Utils

runSwiftColorPrintTests :: FilePath -> IO ()
runSwiftColorPrintTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/test/Swift/Tests/Colors/OutputFiles/"
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
testFileStructures = [swiftExtensionFromColors sampleAppInfo [],
  swiftExtensionFromColors sampleAppInfo swiftTestColors]

resultsFiles :: [String]
resultsFiles = [emptyResultFile,
  fullResultFile]

emptyResultFile :: String
emptyResultFile = "UIColor+EmptyCategory.swift"

fullResultFile :: String
fullResultFile = "UIColor+MSAColors.swift"

emptyTestFile :: String
emptyTestFile = "UIColor+EmptyCategory.swift.test"

fullTestFile :: String
fullTestFile = "UIColor+MSAColors.swift.test"
