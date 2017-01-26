-- This module will test the method:
-- parseViewFromFile :: FilePath -> IO Either [OWAParseError] OWAView
-- which parses a view from a .view file when given a filepath, looking
-- at the specific case of parsing container views.
-- 
-- It will also test the methods:
-- objcHeaderFromView :: OWAAppInfo -> OWAView -> ObjcFile
-- objcImplementationFromView :: OWAAppInfo -> OWAView -> ObjcFile
-- from Objc.ViewConverter follwed by:
-- printStructureToFile :: ObjcFile -> Doc
-- in Objc.Print, again testing the container views case

module Objc.Tests.Views.Containers (
  runContainerViewPrintTests
) where

import Test.Hspec

import Model.OWAAppInfo
import Objc.AbSyn
import Objc.Tests.Utils
import Objc.Tests.Views.ContainerObjects
import Objc.ViewConverter

runContainerViewPrintTests :: FilePath -> IO ()
runContainerViewPrintTests currentDirectory = do
  let outputDirectory = currentDirectory ++ outputDirectoryExtension
  hspec $
    beforeAll_ (removeDiffFiles outputDirectory) $
    beforeAll_ (createResultsFiles outputDirectory resultsFiles testFileStructures)
    . afterAll_ (removeResultsFiles outputDirectory resultsFiles) $ do
      containerViewPrintTests outputDirectory
      scrollViewPrintTests outputDirectory

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

scrollViewPrintTests :: FilePath -> Spec
scrollViewPrintTests outputDirectory = describe "Print File Structure for views with scroll views" $ do
  context "When the scroll view goes vertically" $ do
    it "The printed header should match" $
      (outputDirectory ++ verticalScrollHeaderResultFile) `filesShouldMatch` (outputDirectory ++ verticalScrollHeaderTestFile)

    it "When the scroll view goes horizontally" $ 
      (outputDirectory ++ verticalScrollMResultFile) `filesShouldMatch` (outputDirectory ++ verticalScrollMTestFile)

  context "When there are two nested container views" $ do
    it "The printed header should match" $
      (outputDirectory ++ horizontalScrollHeaderResultFile) `filesShouldMatch` (outputDirectory ++ horizontalScrollHeaderTestFile)

    it "The printed implementation should match" $
      (outputDirectory ++ horizontalScrollMResultFile) `filesShouldMatch` (outputDirectory ++ horizontalScrollMTestFile)

  context "When the scroll view scrolls in both directions" $ do
    it "The printed header should match" $
      (outputDirectory ++ bothScrollHeaderResultFile) `filesShouldMatch` (outputDirectory ++ bothScrollHeaderTestFile)
      
    it "The printed implementation should match" $
      (outputDirectory ++ bothScrollMResultFile) `filesShouldMatch` (outputDirectory ++ bothScrollMTestFile)

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
  objcImplementationFromView sampleAppInfo twoContainersTest,
  objcHeaderFromView sampleAppInfo scrollViewVerticalTestView,
  objcImplementationFromView sampleAppInfo scrollViewVerticalTestView,
  objcHeaderFromView sampleAppInfo scrollViewHorizontalTestView,
  objcImplementationFromView sampleAppInfo scrollViewHorizontalTestView,
  objcHeaderFromView sampleAppInfo scrollViewBothTestView,
  objcImplementationFromView sampleAppInfo scrollViewBothTestView]

resultsFiles :: [String]
resultsFiles = [basicHeaderResultFile,
  basicMResultFile,
  nestedHeaderResultFile,
  nestedMResultFile,
  twoContainersHeaderResultFile,
  twoContainersMResultFile,
  verticalScrollHeaderResultFile,
  verticalScrollMResultFile,
  horizontalScrollHeaderResultFile,
  horizontalScrollMResultFile,
  bothScrollHeaderResultFile,
  bothScrollMResultFile]

outputDirectoryExtension :: String
outputDirectoryExtension = "/test/Objc/Tests/Views/OutputFiles"

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

verticalScrollHeaderResultFile :: String
verticalScrollHeaderResultFile = "/IGAScrollView1.h"

verticalScrollMResultFile :: String
verticalScrollMResultFile = "/IGAScrollView1.m"

horizontalScrollHeaderResultFile :: String
horizontalScrollHeaderResultFile = "/IGAScrollView2.h"

horizontalScrollMResultFile :: String
horizontalScrollMResultFile = "/IGAScrollView2.m"

bothScrollHeaderResultFile :: String
bothScrollHeaderResultFile = "/IGAScrollView3.h"

bothScrollMResultFile :: String
bothScrollMResultFile = "/IGAScrollView3.m"

verticalScrollHeaderTestFile :: String
verticalScrollHeaderTestFile = "/IGAScrollView1.h.test"

verticalScrollMTestFile :: String
verticalScrollMTestFile = "/IGAScrollView1.m.test"

horizontalScrollHeaderTestFile :: String
horizontalScrollHeaderTestFile = "/IGAScrollView2.h.test"

horizontalScrollMTestFile :: String
horizontalScrollMTestFile = "/IGAScrollView2.m.test"

bothScrollHeaderTestFile :: String
bothScrollHeaderTestFile = "/IGAScrollView3.h.test"

bothScrollMTestFile :: String
bothScrollMTestFile = "/IGAScrollView3.m.test"
