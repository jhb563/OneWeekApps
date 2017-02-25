-- Objc.ModelConverter will expose the functions:
-- objcHeaderFromModel :: OWAAppInfo -> OWAModel -> ObjcFile
-- objcImplementationFromModel :: OWAAppInfo -> OWAView -> ObjcFile
-- which each take an appInfo object and a view and return a
-- file structure of objective C statements
--
-- Objc.Print will expose the method:
-- printStructureToFile :: ObjcFile -> FilePath -> IO ()
-- which takes an objective C file structure and a filepath
-- and prints the file structure ot the give file
--
-- These tests will first create the file structures and then
-- print them, testing the printed files.

module Objc.Tests.Models.Basic (
  runModelPrintTests
) where

import Test.Hspec

import Model.OWAAppInfo
import Objc.AbSyn
import Objc.Tests.Utils
import Objc.Tests.Models.Objects
import Objc.ModelConverter

runModelPrintTests :: FilePath -> IO ()
runModelPrintTests _ = print "Model Print tests stubbed out!"

runModelPrintTests' :: FilePath -> IO ()
runModelPrintTests' currentDirectory = do
  let testDirectory = currentDirectory ++ "/test/Objc/Tests/Models/OutputFiles/"
  hspec $
    beforeAll_ (removeDiffFiles testDirectory) $
    beforeAll_ (createResultsFiles testDirectory resultsFiles testFileStructures)
    . afterAll_ (removeResultsFiles testDirectory resultsFiles) $ do
      basicTest testDirectory
      customTest testDirectory
      optionalsTest testDirectory
      arraysTest testDirectory
      mapsTest testDirectory
      completeTest testDirectory

basicTest :: String -> Spec
basicTest testDirectory = describe "Print File Structure for a basic model" $ do
  it "The printed header should match" $
    (testDirectory ++ basicHeaderResultFile) `filesShouldMatch` (testDirectory ++ basicHeaderTestFile)

  it "The printed implementation should match" $
    (testDirectory ++ basicMResultFile) `filesShouldMatch` (testDirectory ++ basicMTestFile)

customTest :: String -> Spec
customTest testDirectory = describe "Print File Structure for a custom model" $ do
  it "The printed header should match" $
    (testDirectory ++ customHeaderResultFile) `filesShouldMatch` (testDirectory ++ customHeaderTestFile)

  it "The printed implementation should match" $
    (testDirectory ++ customMResultFile) `filesShouldMatch` (testDirectory ++ customMTestFile)

optionalsTest :: String -> Spec
optionalsTest testDirectory = describe "Print File Structure for an optionals model" $ do
  it "The printed header should match" $
    (testDirectory ++ optionalsHeaderResultFile) `filesShouldMatch` (testDirectory ++ optionalsHeaderTestFile)

  it "The printed implementation should match" $
    (testDirectory ++ optionalsMResultFile) `filesShouldMatch` (testDirectory ++ optionalsMTestFile)

arraysTest :: String -> Spec
arraysTest testDirectory = describe "Print File Structure for an arrays model" $ do
  it "The printed header should match" $
    (testDirectory ++ arraysHeaderResultFile) `filesShouldMatch` (testDirectory ++ arraysHeaderTestFile)

  it "The printed implementation should match" $
    (testDirectory ++ arraysMResultFile) `filesShouldMatch` (testDirectory ++ arraysMTestFile)

mapsTest :: String -> Spec
mapsTest testDirectory = describe "Print File Structure for a maps model" $ do
  it "The printed header should match" $
    (testDirectory ++ mapsHeaderResultFile) `filesShouldMatch` (testDirectory ++ mapsHeaderTestFile)

  it "The printed implementation should match" $
    (testDirectory ++ mapsMResultFile) `filesShouldMatch` (testDirectory ++ mapsMTestFile)

completeTest :: String -> Spec
completeTest testDirectory = describe "Print File Structure for a complete model" $ do
  it "The printed header should match" $
    (testDirectory ++ completeHeaderResultFile) `filesShouldMatch` (testDirectory ++ completeHeaderTestFile)

  it "The printed implementation should match" $
    (testDirectory ++ completeMResultFile) `filesShouldMatch` (testDirectory ++ completeMTestFile)

sampleAppInfo :: OWAAppInfo
sampleAppInfo = OWAAppInfo {
  appName = "MySampleApp",
  appPrefix = "MSA",
  authorName = "James Bowen",
  dateCreatedString = "4/30/2016",
  companyName = Just "One Week Apps"
}

testFileStructures :: [ObjcFile]
testFileStructures =
  [ objcHeaderFromModel sampleAppInfo basicModel
  , objcImplementationFromModel sampleAppInfo basicModel
  , objcHeaderFromModel sampleAppInfo customModel
  , objcImplementationFromModel sampleAppInfo customModel
  , objcHeaderFromModel sampleAppInfo optionalsModel
  , objcImplementationFromModel sampleAppInfo optionalsModel
  , objcHeaderFromModel sampleAppInfo arraysModel
  , objcImplementationFromModel sampleAppInfo arraysModel
  , objcHeaderFromModel sampleAppInfo mapsModel
  , objcImplementationFromModel sampleAppInfo mapsModel
  , objcHeaderFromModel sampleAppInfo completeModel
  , objcImplementationFromModel sampleAppInfo completeModel ]

resultsFiles :: [String]
resultsFiles =
  [ basicHeaderResultFile
  , basicMResultFile
  , customHeaderResultFile
  , customMResultFile
  , optionalsHeaderResultFile
  , optionalsMResultFile
  , arraysHeaderResultFile
  , arraysMResultFile
  , mapsHeaderResultFile
  , mapsMResultFile
  , completeHeaderResultFile
  , completeMResultFile ]

basicHeaderResultFile :: String
basicHeaderResultFile = "MyFirstModel.h"

basicMResultFile :: String
basicMResultFile = "MyFirstModel.m"

customHeaderResultFile :: String
customHeaderResultFile = "CustomReferenceModel.h"

customMResultFile :: String
customMResultFile = "CustomReferenceModel.m"

optionalsHeaderResultFile :: String
optionalsHeaderResultFile = "OptionalsModel.h"

optionalsMResultFile :: String
optionalsMResultFile = "OptionalsModel.m"

arraysHeaderResultFile :: String
arraysHeaderResultFile = "ArrayObject.h"

arraysMResultFile :: String
arraysMResultFile = "ArrayObject.m"

mapsHeaderResultFile :: String
mapsHeaderResultFile = "MapObject.h"

mapsMResultFile :: String
mapsMResultFile = "MapObject.m"

completeHeaderResultFile :: String
completeHeaderResultFile = "MyCompleteModel.h"

completeMResultFile :: String
completeMResultFile = "MyCompleteModel.m"

basicHeaderTestFile :: String
basicHeaderTestFile = "MyFirstModel.h.test"

basicMTestFile :: String
basicMTestFile = "MyFirstModel.m.test"

customHeaderTestFile :: String
customHeaderTestFile = "CustomReferenceModel.h.test"

customMTestFile :: String
customMTestFile = "CustomReferenceModel.m.test"

optionalsHeaderTestFile :: String
optionalsHeaderTestFile = "OptionalsModel.h.test"

optionalsMTestFile :: String
optionalsMTestFile = "OptionalsModel.m.test"

arraysHeaderTestFile :: String
arraysHeaderTestFile = "ArrayObject.h.test"

arraysMTestFile :: String
arraysMTestFile = "ArrayObject.m.test"

mapsHeaderTestFile :: String
mapsHeaderTestFile = "MapObject.h.test"

mapsMTestFile :: String
mapsMTestFile = "MapObject.m.test"

completeHeaderTestFile :: String
completeHeaderTestFile = "MyCompleteModel.h.test"

completeMTestFile :: String
completeMTestFile = "MyCompleteModel.m"
