-- This module will test the method:
-- parseViewFromFile :: FilePath -> IO Either [OWAParseError] OWAView
-- which parses a view from a .view file when given a filepath, looking
-- at the specific case of parsing container views.
-- 
-- It will also test the methods:
-- objcHeaderFromView :: OWAAppInfo -> OWAView -> ObjcFile
-- objcImplementationFromView :: OWAAppInfo -> OWAView -> ObjcFile
-- from OWAViewObjc follwed by:
-- printStructureToFile :: ObjcFile -> Doc
-- in OWAObjcPrint, again testing the container views case

module ContainerViewTests (
  runContainerViewTests
) where

import OWAAppInfo
import OWAObjcAbSyn
import OWAViewObjc
import OWAViewParser
import TestUtil
import TestContainerViews
import Test.Hspec

runContainerViewTests :: FilePath -> IO ()
runContainerViewTests currentDirectory = do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  let outputDirectory = currentDirectory ++ outputDirectoryExtension
  hspec $
    beforeAll_ (removeDiffFiles outputDirectory) $
    beforeAll_ (createResultsFiles outputDirectory resultsFiles testFileStructures)
    . afterAll_ (removeResultsFiles outputDirectory resultsFiles) $ do
      containerViewParseTests parseDirectory
      containerViewPrintTests outputDirectory
      scrollViewParseTests parseDirectory

containerViewParseTests :: FilePath -> Spec
containerViewParseTests parseDirectory = do
  let testFile1 = parseDirectory ++ basicParseExtension
  let testFile2 = parseDirectory ++ nestedParseExtension
  let testFile3 = parseDirectory ++ twoContainersParseExtension
  describe "Parse Views with ContainerView elements" $ do
    context "When there is a single ContainerView" $
      it "Should match the test view" $
        parseViewFromFile testFile1 `shouldReturnRights` basicContainerTest
    
    context "When there are two of the same type of ContainerView" $
      it "Should match the test view" $
        parseViewFromFile testFile2 `shouldReturnRights` nestedContainerTest

    context "When there are two different types of ContainerViews" $
      it "Should match the test view" $
        parseViewFromFile testFile3 `shouldReturnRights` twoContainersTest

scrollViewParseTests :: FilePath -> Spec
scrollViewParseTests parseDirectory = do
  let testFile1 = parseDirectory ++ scrollViewDefaultTestExtension
  let testFile2 = parseDirectory ++ scrollViewVerticalTestExtension
  let testFile3 = parseDirectory ++ scrollViewHorizontalTestExtension
  let testFile4 = parseDirectory ++ scrollViewBothTestExtension
  describe "Parse Views with ScrollView elements" $ do
    context "When the scroll view has no direction specified" $
      it "Should match the vertical test view" $
        parseViewFromFile testFile1 `shouldReturnRights` scrollViewDefaultTestView
    
    context "When the scoll view has a vertical direction" $
      it "Should match the vertical test view" $
        parseViewFromFile testFile2 `shouldReturnRights` scrollViewVerticalTestView

    context "When the scroll view has a horizontal direction" $
      it "Should match the horizontal test view" $
        parseViewFromFile testFile3 `shouldReturnRights` scrollViewHorizontalTestView

    context "When the scroll view has both directions" $
      it "Should match the both-direction test view" $
        parseViewFromFile testFile4 `shouldReturnRights` scrollViewBothTestView

containerViewPrintTests :: FilePath -> Spec
containerViewPrintTests outputDirectory = describe "Print File Structure for views with container views" $ do
  context "When there is a single container view" $ do
    it "The printed header should match" $
      (outputDirectory ++ basicHeaderResultFile) `filesShouldMatch` (outputDirectory ++ basicHeaderTestFile)

    it "The printed implementation should match" $ 
      (outputDirectory ++ basicMResultFile) `filesShouldMatch` (outputDirectory ++ basicMTestFile)

  context "When there are two nested container views" $ do
    it "The printed header should match" $
      (outputDirectory ++ nestedHeaderResultFile) `filesShouldMatch` (outputDirectory ++ nestedHeaderTestFile)

    it "The printed implementation should match" $
      (outputDirectory ++ nestedMResultFile) `filesShouldMatch` (outputDirectory ++ nestedMTestFile)

  context "When there are two container views amongst other views" $ do
    it "The printed header should match" $
      (outputDirectory ++ twoContainersHeaderResultFile) `filesShouldMatch` (outputDirectory ++ twoContainersHeaderTestFile)
      
    it "The printed implementation should match" $
      (outputDirectory ++ twoContainersMResultFile) `filesShouldMatch` (outputDirectory ++ twoContainersMTestFile)

sampleAppInfo :: OWAAppInfo
sampleAppInfo = OWAAppInfo {
  appName = "MySampleApp",
  appPrefix = "MSA",
  authorName = "James Bowen",
  dateCreatedString = "4/30/2016",
  companyName = Just "One Week Apps"
}

testFileStructures :: [ObjcFile]
testFileStructures = [objcHeaderFromView sampleAppInfo basicContainerTest,
  objcImplementationFromView sampleAppInfo basicContainerTest,
  objcHeaderFromView sampleAppInfo nestedContainerTest,
  objcImplementationFromView sampleAppInfo nestedContainerTest,
  objcHeaderFromView sampleAppInfo twoContainersTest,
  objcImplementationFromView sampleAppInfo twoContainersTest]

resultsFiles :: [String]
resultsFiles = [basicHeaderResultFile,
  basicMResultFile,
  nestedHeaderResultFile,
  nestedMResultFile,
  twoContainersHeaderResultFile,
  twoContainersMResultFile]

parseDirectoryExtension :: String
parseDirectoryExtension = "/tests/Version021Tests/ContainerViewTests/ParseFiles"

basicParseExtension :: String
basicParseExtension = "/basicContainerTest.view"

nestedParseExtension :: String
nestedParseExtension = "/nestedContainerTest.view"

twoContainersParseExtension :: String
twoContainersParseExtension = "/twoContainersTest.view"

scrollViewDefaultTestExtension :: String
scrollViewDefaultTestExtension = "/scrollViewDefaultTest.view"

scrollViewVerticalTestExtension :: String
scrollViewVerticalTestExtension = "/scrollViewVerticalTest.view"

scrollViewHorizontalTestExtension :: String
scrollViewHorizontalTestExtension = "/scrollViewHorizontalTest.view"

scrollViewBothTestExtension :: String
scrollViewBothTestExtension = "/scrollViewBothTest.view"

outputDirectoryExtension :: String
outputDirectoryExtension = "/tests/Version021Tests/ContainerViewTests/OutputFiles"

basicHeaderResultFile :: String
basicHeaderResultFile = "/IGAContainerView1.h"

basicMResultFile :: String
basicMResultFile = "/IGAContainerView1.m"

nestedHeaderResultFile :: String
nestedHeaderResultFile = "/IGAContainerView2.h"

nestedMResultFile :: String
nestedMResultFile = "/IGAContainerView2.m"

twoContainersHeaderResultFile :: String
twoContainersHeaderResultFile = "/IGAContainerView3.h"

twoContainersMResultFile :: String
twoContainersMResultFile = "/IGAContainerView3.m"

basicHeaderTestFile :: String
basicHeaderTestFile = "/IGAContainerView1.h.test"

basicMTestFile :: String
basicMTestFile = "/IGAContainerView1.m.test"

nestedHeaderTestFile :: String
nestedHeaderTestFile = "/IGAContainerView2.h.test"

nestedMTestFile :: String
nestedMTestFile = "/IGAContainerView2.m.test"

twoContainersHeaderTestFile :: String
twoContainersHeaderTestFile = "/IGAContainerView3.h.test"

twoContainersMTestFile :: String
twoContainersMTestFile = "/IGAContainerView3.m.test"
