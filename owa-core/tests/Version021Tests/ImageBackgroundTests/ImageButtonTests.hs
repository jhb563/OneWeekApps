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
import Parse.ViewParser
import TestButtonObjects
import TestUtil

runImageButtonTests :: FilePath -> IO ()
runImageButtonTests currentDirectory = do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  let outputDirectory = currentDirectory ++ outputDirectoryExtension
  hspec $
    beforeAll_ (removeDiffFiles outputDirectory) $
    beforeAll_ (createResultsFiles outputDirectory resultsFiles testFileStructures)
    . afterAll_ (removeResultsFiles outputDirectory resultsFiles) $ do
      imageButtonParseTests parseDirectory
      imageButtonPrintTests outputDirectory

imageButtonParseTests :: FilePath -> Spec
imageButtonParseTests parseDirectory = do
  let testFile1 = parseDirectory ++ imageButtonExtension1
  let testFile2 = parseDirectory ++ imageButtonExtension2
  let testFile3 = parseDirectory ++ imageButtonExtension3
  describe "Parse view files where buttons use image source tag" $ do
    context "When the property is used correctly" $
      it "Should parse the view correctly with an image for the button" $ 
        parseViewFromFile testFile1 `shouldReturnRights` testSuccessView

    context "When the image file name is improperly not in quotations" $ 
      it "Should return a parse error highlighting the improper name" $
        parseViewFromFile testFile2 `shouldMatchError` missingQuotesError

    context "When the wrong tag is used instead of ImageSrc" $
      it "Should return a parse error highlighting the improper tag" $
        parseViewFromFile testFile3 `shouldMatchError` wrongTagNameError

imageButtonPrintTests :: FilePath -> Spec
imageButtonPrintTests outputDirectory = describe "Print File Structure for view with an image button" $ do
  it "The printed header should match" $
    (outputDirectory ++ imageButtonHeaderResultFile) `filesShouldMatch` (outputDirectory ++ imageButtonHeaderTestFile)

  it "The printed implementation should match" $
    (outputDirectory ++ imageButtonMResultFile) `filesShouldMatch` (outputDirectory ++ imageButtonMTestFile)

parseDirectoryExtension :: String
parseDirectoryExtension = "/tests/Version021Tests/ImageBackgroundTests/ParseFiles"

imageButtonExtension1 :: String
imageButtonExtension1 = "/imageButtonView.view"

imageButtonExtension2 :: String
imageButtonExtension2 = "/imageButtonError1.view"

imageButtonExtension3 :: String
imageButtonExtension3 = "/imageButtonError2.view"

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
