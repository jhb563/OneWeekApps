-- This module will test the method:
-- parseViewFromFile :: FilePath -> IO Either [OWAParseError] OWAView
-- which parses a view from a .view file when given a filepath, looking
-- at the specific case of buttons using an image for a background.
-- 
-- It will also test the methods:
-- objcHeaderFromView :: OWAAppInfo -> OWAView -> ObjcFile
-- objcImplementationFromView :: OWAAppInfo -> OWAView -> ObjcFile
-- from Objc.ViewConverter follwed by:
-- printStructureToFile :: ObjcFile -> Doc
-- in Objc.Print, again testing the image button case

module ImageButtonTests (
  runImageButtonTests
) where

import Test.Hspec

import Model.OWAAppInfo
import Objc.AbSyn
import Objc.ViewConverter
import TestButtonObjects
import TestUtil

runImageButtonTests :: FilePath -> IO ()
runImageButtonTests currentDirectory = do
  let outputDirectory = currentDirectory ++ outputDirectoryExtension
  hspec $
    beforeAll_ (removeDiffFiles outputDirectory) $
    beforeAll_ (createResultsFiles outputDirectory resultsFiles testFileStructures)
    . afterAll_ (removeResultsFiles outputDirectory resultsFiles) $ do
      imageButtonPrintTests outputDirectory

imageButtonPrintTests :: FilePath -> Spec
imageButtonPrintTests outputDirectory = describe "Print File Structure for view with an image button" $ do
  it "The printed header should match" $
    (outputDirectory ++ imageButtonHeaderResultFile) `filesShouldMatch` (outputDirectory ++ imageButtonHeaderTestFile)

  it "The printed implementation should match" $
    (outputDirectory ++ imageButtonMResultFile) `filesShouldMatch` (outputDirectory ++ imageButtonMTestFile)

outputDirectoryExtension :: String
outputDirectoryExtension = "/tests/Version021Tests/ImageBackgroundTests/OutputFiles"

sampleAppInfo :: OWAAppInfo
sampleAppInfo = OWAAppInfo {
  appName = "MySampleApp",
  appPrefix = "MSA",
  authorName = "James Bowen",
  dateCreatedString = "4/30/2016",
  companyName = Just "One Week Apps"
}

testFileStructures :: [ObjcFile]
testFileStructures = [objcHeaderFromView sampleAppInfo testSuccessView,
                     objcImplementationFromView sampleAppInfo testSuccessView]

resultsFiles :: [String]
resultsFiles = [imageButtonHeaderResultFile, imageButtonMResultFile]

imageButtonHeaderTestFile :: String
imageButtonHeaderTestFile = "/OWAImageButtonView.h.test"

imageButtonMTestFile :: String
imageButtonMTestFile = "/OWAImageButtonView.m.test"

imageButtonHeaderResultFile :: String
imageButtonHeaderResultFile = "/OWAImageButtonView.h"

imageButtonMResultFile :: String
imageButtonMResultFile = "/OWAImageButtonView.m"
