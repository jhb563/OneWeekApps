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
      viewParseTests parseDirectory
      viewPrintTests outputDirectory

viewParseTests :: FilePath -> Spec
viewParseTests parseDirectory = do
  let testFile1 = parseDirectory ++ basicParseExtension
  let testFile2 = parseDirectory ++ nestedParseExtension
  let testFile3 = parseDirectory ++ twoContainersParseExtension
  describe "Parse Views with ContainerView elements" $ do
    context "When there is a single ContainerView" $
      it "Should match the test view" $
        parseViewFromFile testFile1 `shouldReturnRights` basicContainerTest
    
    context "When there are two of the same type of CustomView" $
      it "Should match the test view" $
        parseViewFromFile testFile2 `shouldReturnRights` nestedContainerTest

    context "When there are two different types of CustomViews" $
      it "Should match the test view" $
        parseViewFromFile testFile3 `shouldReturnRights` twoContainersTest

viewPrintTests :: FilePath -> Spec
viewPrintTests outputDirectory = describe "Print File Structure for views with container views" $ do
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
