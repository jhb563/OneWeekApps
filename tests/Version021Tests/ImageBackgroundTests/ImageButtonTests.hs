-- This module will test the method:
-- parseViewFromFile :: FilePath -> IO Either [OWAParseError] OWAView
-- which parses a view from a .view file when given a filepath, looking
-- at the specific case of buttons using an image for a background.
-- 
-- It will also test the methods:
-- objcHeaderFromView :: OWAAppInfo -> OWAView -> ObjcFile
-- objcImplementationFromView :: OWAAppInfo -> OWAView -> ObjcFile
-- from OWAViewObjc follwed by:
-- printStructureToFile :: ObjcFile -> Doc
-- in OWAObjcPrint, again testing the image button case

module ImageButtonTests (
  runImageButtonTests
) where

--import OWAAppInfo
import OWAObjcAbSyn
--import OWAViewObjc
import OWAViewParser
import TestButtonObjects
import TestUtil
import Test.Hspec

runImageButtonTests :: FilePath -> IO ()
runImageButtonTests currentDirectory = do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  let outputDirectory = currentDirectory ++ outputDirectoryExtension
  hspec $
    beforeAll_ (removeDiffFiles outputDirectory) $
    beforeAll_ (createResultsFiles outputDirectory resultsFiles testFileStructures)
    . afterAll_ (removeResultsFiles outputDirectory resultsFiles) $ do
      imageButtonParseTests parseDirectory

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

resultsFiles :: [String]
resultsFiles = []

testFileStructures :: [ObjcFile]
testFileStructures = []
