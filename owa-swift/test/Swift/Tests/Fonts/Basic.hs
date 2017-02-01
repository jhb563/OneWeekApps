-- Swift.FontConverter will expose the method:
-- swiftExtensionFromFonts :: OWAAppInfo -> [OWAFont] -> SwiftFile
-- which take an appInfo object and a list of fonts and return a
-- file structure of Swift statements
--
-- Swift.Print will expose the method
-- printStructureToFile :: SwiftFile -> FilePath -> IO ()
-- which takes a Swift file structure and a filepath and
-- prints the file structure to the given file
--
-- These tests will first create the file structures and then
-- print them, testing the printed files.

module Swift.Tests.Fonts.Basic (
  runSwiftFontPrintTests
) where

import Test.Hspec

import Model.OWAAppInfo
import Swift.AbSyn
import Swift.FontConverter
import Swift.Tests.Fonts.Objects
import Swift.Tests.Utils

runSwiftFontPrintTests :: FilePath -> IO ()
runSwiftFontPrintTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/test/Swift/Tests/Fonts/OutputFiles/"
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
testFileStructures = [swiftExtensionFromFonts sampleAppInfo [],
  swiftExtensionFromFonts sampleAppInfo swiftTestFonts]

resultsFiles :: [String]
resultsFiles = [emptyResultFile,
  fullResultFile]

emptyResultFile :: String
emptyResultFile = "UIFont+EmptyCategory.swift"

fullResultFile :: String
fullResultFile = "UIFont+MSAFonts.swift"

emptyTestFile :: String
emptyTestFile = "UIFont+EmptyCategory.swift.test"

fullTestFile :: String
fullTestFile = "UIFont+MSAFonts.swift.test"
