-- This module will test the method:
-- parseViewFromFile :: FilePath -> IO Either [OWAParseError] OWAView
-- which parses a view from a .view file when given a filepath, looking
-- at the specific case of parsing custom views.
-- 
-- It will also test the methods:
-- objcHeaderFromView :: OWAAppInfo -> OWAView -> ObjcFile
-- objcImplementationFromView :: OWAAppInfo -> OWAView -> ObjcFile
-- from Objc.ViewConverter follwed by:
-- printStructureToFile :: ObjcFile -> Doc
-- in Objc.Print, again testing the custom views case

module CustomViewTests (
  runCustomViewTests
) where

import Test.Hspec

import Model.OWAAppInfo
import Objc.AbSyn
import Objc.ViewConverter
import TestCustomViews
import TestUtil

runCustomViewTests :: FilePath -> IO ()
runCustomViewTests currentDirectory = do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  let outputDirectory = currentDirectory ++ outputDirectoryExtension
  hspec $
    beforeAll_ (removeDiffFiles outputDirectory) $
    beforeAll_ (createResultsFiles outputDirectory resultsFiles testFileStructures)
    . afterAll_ (removeResultsFiles outputDirectory resultsFiles) $
      viewPrintTests outputDirectory

viewPrintTests :: FilePath -> Spec
viewPrintTests outputDirectory = describe "Print File Structure for views with custom views" $ do
  context "When there is a single custom view" $ do
    it "The printed header should match" $
      (outputDirectory ++ basicHeaderResultFile) `filesShouldMatch` (outputDirectory ++ basicHeaderTestFile)

    it "The printed implementation should match" $ 
      (outputDirectory ++ basicMResultFile) `filesShouldMatch` (outputDirectory ++ basicMTestFile)

  context "When there are two custom subvies of the same type" $ do
    it "The printed header should match" $
      (outputDirectory ++ twoSameHeaderResultFile) `filesShouldMatch` (outputDirectory ++ twoSameHeaderTestFile)

    it "The printed implementation should match" $
      (outputDirectory ++ twoSameMResultFile) `filesShouldMatch` (outputDirectory ++ twoSameMTestFile)

  context "When there are two custom views of different types" $ do
    it "The printed header should match" $
      (outputDirectory ++ twoDifferentHeaderResultFile) `filesShouldMatch` (outputDirectory ++ twoDifferentHeaderTestFile)
      
    it "The printed implementation should match" $
      (outputDirectory ++ twoDifferentMResultFile) `filesShouldMatch` (outputDirectory ++ twoDifferentMTestFile)

sampleAppInfo :: OWAAppInfo
sampleAppInfo = OWAAppInfo {
  appName = "MySampleApp",
  appPrefix = "MSA",
  authorName = "James Bowen",
  dateCreatedString = "4/30/2016",
  companyName = Just "One Week Apps"
}

testFileStructures :: [ObjcFile]
testFileStructures = [objcHeaderFromView sampleAppInfo basicCustomTest,
  objcImplementationFromView sampleAppInfo basicCustomTest,
  objcHeaderFromView sampleAppInfo twoSameCustomTest,
  objcImplementationFromView sampleAppInfo twoSameCustomTest,
  objcHeaderFromView sampleAppInfo twoDifferentCustomTest,
  objcImplementationFromView sampleAppInfo twoDifferentCustomTest]

resultsFiles :: [String]
resultsFiles = [basicHeaderResultFile,
  basicMResultFile,
  twoSameHeaderResultFile,
  twoSameMResultFile,
  twoDifferentHeaderResultFile,
  twoDifferentMResultFile]

parseDirectoryExtension :: String
parseDirectoryExtension = "/tests/Version021Tests/CustomViewTests/ParseFiles"

outputDirectoryExtension :: String
outputDirectoryExtension = "/tests/Version021Tests/CustomViewTests/OutputFiles"

basicHeaderResultFile :: String
basicHeaderResultFile = "/IGACustomTest1.h"

basicMResultFile :: String
basicMResultFile = "/IGACustomTest1.m"

twoSameHeaderResultFile :: String
twoSameHeaderResultFile = "/IGACustomTest2.h"

twoSameMResultFile :: String
twoSameMResultFile = "/IGACustomTest2.m"

twoDifferentHeaderResultFile :: String
twoDifferentHeaderResultFile = "/IGACustomTest3.h"

twoDifferentMResultFile :: String
twoDifferentMResultFile = "/IGACustomTest3.m"

basicHeaderTestFile :: String
basicHeaderTestFile = "/IGACustomTest1.h.test"

basicMTestFile :: String
basicMTestFile = "/IGACustomTest1.m.test"

twoSameHeaderTestFile :: String
twoSameHeaderTestFile = "/IGACustomTest2.h.test"

twoSameMTestFile :: String
twoSameMTestFile = "/IGACustomTest2.m.test"

twoDifferentHeaderTestFile :: String
twoDifferentHeaderTestFile = "/IGACustomTest3.h.test"

twoDifferentMTestFile :: String
twoDifferentMTestFile = "/IGACustomTest3.m.test"
