-- OWAStringsObjc exposes the method:
-- objcStringsFileFromStringSets :: OWAAppInfo -> [OWALocalizedStringSet] -> ObjcFile
-- which takes an appInfo object and a list of string sets and returns
-- a file structure of objective C statements.
--
-- OWAObjcPrint exposes the method:
-- printStructureToFile :: ObjcFile -> FilePath -> IO ()
-- which takes an objective C file structure and a filepath
-- and prints the file structure to the given file.
--
-- These tests will first create the file structures and then print them,
-- testing the printed files. 

module StringsPrintTests (
  runStringsPrintTests
) where

import OWAAppInfo
import OWAObjcAbSyn
import OWAStringsObjc
import TestStringSets
import TestUtil
import Test.Hspec

runStringsPrintTests :: FilePath -> IO ()
runStringsPrintTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/tests/Version015Tests/StringsTests/StringsOutputFiles/"
  hspec $
    beforeAll_ (removeDiffFiles testDirectory) $
    beforeAll_ (createResultsFiles testDirectory resultsFiles testFileStructures)
    . afterAll_ (removeResultsFiles testDirectory resultsFiles) $ do
      singleFileTests testDirectory
      multiFileTests testDirectory

singleFileTests :: FilePath -> Spec
singleFileTests testDirectory = describe "Print File Structure for single string set" $ do
  it "The printed basic strings file should match" $
    (testDirectory ++ basicResultFile) `filesShouldMatch` (testDirectory ++ basicTestFile)

  it "The printed quoted strings file should match" $
    (testDirectory ++ quotedResultFile) `filesShouldMatch` (testDirectory ++ quotedTestFile)

  it "The printed spaced strings file should match" $
    (testDirectory ++ spacedResultFile) `filesShouldMatch` (testDirectory ++ spacedTestFile)

  it "The printed commented strings file should match" $
    (testDirectory ++ commentedResultFile) `filesShouldMatch`
      (testDirectory ++ commentedTestFile)

multiFileTests :: FilePath -> Spec
multiFileTests testDirectory  = describe "Print File Structure for multiple string sets" $ do
  it "The printed two file strings file should match" $
    (testDirectory ++ twoFileResultFile) `filesShouldMatch` (testDirectory ++ twoFileTestFile)

  it "The printed all strings file should match" $
    (testDirectory ++ allStringsResultFile) `filesShouldMatch` 
      (testDirectory ++ allStringsTestFile)

sampleAppInfo :: OWAAppInfo
sampleAppInfo = OWAAppInfo {
  appName = "StringsTestApp",
  appPrefix = "STA",
  authorName = "James Bowen",
  dateCreatedString = "3/25/2016",
  companyName = "One Week Apps"
}

testFileStructures :: [ObjcFile]
testFileStructures = [objcStringsFileFromStringSets sampleAppInfo [basicStrings],
  objcStringsFileFromStringSets sampleAppInfo [quotedStrings],
  objcStringsFileFromStringSets sampleAppInfo [spacedStrings],
  objcStringsFileFromStringSets sampleAppInfo [commentedStrings],
  objcStringsFileFromStringSets sampleAppInfo [basicStrings, quotedStrings],
  objcStringsFileFromStringSets sampleAppInfo [basicStrings, quotedStrings, spacedStrings, comentedStrings]]

resultsFiles :: [String]
resultsFiles = [basicResultFile,
  quotedResultFile,
  spacedResultFile,
  commentedResultFile,
  twoFileResultFile,
  allStringsResultFile]

basicResultFile :: String
basicResultFile = "basicStrings.strings"

quotedResultFile :: String
quotedResultFile = "quotedStrings.strings"

spacedResultFile :: String
spacedResultFile = "spacedStrings.strings"

commentedResultFile :: String
commentedResultFile = "commentedStrings.strings"

twoFileResultFile :: String
twoFileResultFile = "twoFile.strings"

allStringsResultFile :: String
allStringsResultFile = "allStrings.strings"

basicTestFile :: String
basicTestFile = "basicStrings.strings.test"

quotedTestFile :: String
quotedTestFile = "quotedStrings.strings.test"

spacedTestFile :: String
spacedTestFile = "spacedStrings.strings.test"

commentedTestFile :: String
commentedTestFile = "commentedStrings.strings.test"

twoFileTestFile :: String
twoFileTestFile = "twoFile.strings.test"

allStringsTestFile :: String
allStringsTestFile = "allStrings.strings.test"
