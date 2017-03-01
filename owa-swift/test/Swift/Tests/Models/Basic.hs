-- Swift.ModelConverter will expose the function:
-- swiftFileFromModel :: OWAAppInfo -> OWAModel -> SwiftFile
-- which takes an appInfo object and a model and returns a
-- file structure of Swift statements.
--
-- Swift.Print will expose the method:
-- printStructureToFile :: SwiftFile -> FilePath -> IO ()
-- which takes an Swift file structure and a filepath
-- and prints the file structure to the give file
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

runSwiftModelPrintTests' :: FilePath -> IO ()
runSwiftModelPrintTests' currentDirectory = do
  let testDirectory = currentDirectory ++ "/test/Swift/Tests/Models/OutputFiles/"
  hspec $
    beforeAll_ (removeDiffFiles testDirectory) $
    beforeAll_ (createSwiftResultsFiles testDirectory resultsFiles testFileStructures)
    . afterAll_ (removeResultsFiles testDirectory resultsFiles) $ do
      basicTest testDirectory
      customTest testDirectory
      optionalsTest testDirectory
      arraysTest testDirectory
      mapsTest testDirectory
      completeTest testDirectory

basicTest :: String -> Spec
basicTest testDirectory = describe "Print File Structure for a basic model" $ do
  it "The printed swift file should match" $
    (testDirectory ++ basicResultFile) `filesShouldMatch` (testDirectory ++ basicTestFile)

customTest :: String -> Spec
customTest testDirectory = describe "Print File Structure for a custom model" $ do
  it "The printed swift file should match" $
    (testDirectory ++ customResultFile) `filesShouldMatch` (testDirectory ++ customTestFile)

optionalsTest :: String -> Spec
optionalsTest testDirectory = describe "Print File Structure for an optionals model" $ do
  it "The printed swift file should match" $
    (testDirectory ++ optionalsResultFile) `filesShouldMatch` (testDirectory ++ optionalsTestFile)

arraysTest :: String -> Spec
arraysTest testDirectory = describe "Print File Structure for an arrays model" $ do
  it "The printed swift file should match" $
    (testDirectory ++ arraysResultFile) `filesShouldMatch` (testDirectory ++ arraysTestFile)

mapsTest :: String -> Spec
mapsTest testDirectory = describe "Print File Structure for a maps model" $ do
  it "The printed swift file should match" $
    (testDirectory ++ mapsResultFile) `filesShouldMatch` (testDirectory ++ mapsTestFile)

completeTest :: String -> Spec
completeTest testDirectory = describe "Print File Structure for a complete model" $ do
  it "The printed swift file should match" $
    (testDirectory ++ completeResultFile) `filesShouldMatch` (testDirectory ++ completeTestFile)

sampleAppInfo :: OWAAppInfo
sampleAppInfo = OWAAppInfo {
  appName = "MySampleApp",
  appPrefix = "MSA",
  authorName = "James Bowen",
  dateCreatedString = "4/30/2016",
  companyName = Just "One Week Apps"
}

testFileStructures :: [SwiftFile]
testFileStructures =
  [ swiftFileFromModel sampleAppInfo basicModel
  , swiftFileFromModel sampleAppInfo customModel
  , swiftFileFromModel sampleAppInfo optionalsModel
  , swiftFileFromModel sampleAppInfo arraysModel
  , swiftFileFromModel sampleAppInfo mapsModel
  , swiftFileFromModel sampleAppInfo completeModel ]

resultsFiles :: [String]
resultsFiles =
  [ basicResultFile
  , customResultFile
  , optionalsResultFile
  , arraysResultFile
  , mapsResultFile
  , completeResultFile ]

basicResultFile :: String
basicResultFile = "MyFirstModel.swift"

customResultFile :: String
customResultFile = "CustomReferenceModel.swift"

optionalsResultFile :: String
optionalsResultFile = "OptionalsModel.swift"

arraysResultFile :: String
arraysResultFile = "ArrayObject.swift"

mapsResultFile :: String
mapsResultFile = "MapObject.swift"

completeResultFile :: String
completeResultFile = "MyCompleteModel.swift"

basicTestFile :: String
basicTestFile = "MyFirstModel.swift.test"

customTestFile :: String
customTestFile = "CustomReferenceModel.swift.test"

optionalsTestFile :: String
optionalsTestFile = "OptionalsModel.swift.test"

arraysTestFile :: String
arraysTestFile = "ArrayObject.swift.test"

mapsTestFile :: String
mapsTestFile = "MapObject.swift.test"

completeTestFile :: String
completeTestFile = "MyCompleteModel.swift.test"
