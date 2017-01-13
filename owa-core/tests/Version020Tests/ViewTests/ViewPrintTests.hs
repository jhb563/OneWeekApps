-- Objc.ViewConverter will expose the methods:
-- objcHeaderFromView :: OWAAppInfo -> OWAView -> ObjcFile
-- objcImplementationFromView :: OWAAppInfo -> OWAView -> ObjcFile
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

module ViewPrintTests (
  runViewPrintTests
) where

import Test.Hspec

import Model.OWAAppInfo
import Objc.AbSyn
import Objc.ViewConverter
import TestUtil
import TestViews

runViewPrintTests :: FilePath -> IO ()
runViewPrintTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/tests/Version020Tests/ViewTests/ViewOutputFiles/"
  hspec $
    beforeAll_ (removeDiffFiles testDirectory) $
    beforeAll_ (createResultsFiles testDirectory resultsFiles testFileStructures)
    . afterAll_ (removeResultsFiles testDirectory resultsFiles) $ do
      nameTests testDirectory
      elementTests testDirectory
      constraintTests testDirectory

nameTests :: FilePath -> Spec
nameTests testDirectory = describe "Print File Structure for views with no elements or constraints" $ do
    it "The printed header should match for view 1" $
      (testDirectory ++ name1HeaderResultFile) `filesShouldMatch` (testDirectory ++ name1HeaderTestFile)

    it "The printed implementation should match for view 1" $
      (testDirectory ++ name1MResultFile) `filesShouldMatch` (testDirectory ++ name1MTestFile)      

    it "The printed header should match for view 3" $
      (testDirectory ++ name3HeaderResultFile) `filesShouldMatch` (testDirectory ++ name3HeaderTestFile)

    it "The printed implementation should match for view 3" $
      (testDirectory ++ name3MResultFile) `filesShouldMatch` (testDirectory ++ name3MTestFile)

elementTests :: FilePath -> Spec
elementTests testDirectory = describe "Print File Structure for views with elements but not constraints" $ do
    it "The printed header should match for view 1" $
      (testDirectory ++ element1HeaderResultFile) `filesShouldMatch` (testDirectory ++ element1HeaderTestFile)

    it "The printed implementation should match for view 1" $
      (testDirectory ++ element1MResultFile) `filesShouldMatch` (testDirectory ++ element1MTestFile)      

    it "The printed header should match for view 2" $
      (testDirectory ++ element2HeaderResultFile) `filesShouldMatch` (testDirectory ++ element2HeaderTestFile)

    it "The printed implementation should match for view 2" $
      (testDirectory ++ element2MResultFile) `filesShouldMatch` (testDirectory ++ element2MTestFile)

    it "The printed header should match for view 3" $
      (testDirectory ++ element3HeaderResultFile) `filesShouldMatch` (testDirectory ++ element3HeaderTestFile)

    it "The printed implementation should match for view 3" $
      (testDirectory ++ element3MResultFile) `filesShouldMatch` (testDirectory ++ element3MTestFile)

constraintTests :: FilePath -> Spec
constraintTests testDirectory = describe "Print File Structure for views with elements and constraints" $ do
    context "when the constraints are height and width constraints" $ do
      it "The printed header should match" $
        (testDirectory ++ heightWidthHeaderResultFile) `filesShouldMatch` (testDirectory ++ heightWidthHeaderTestFile)

      it "The printed implementation should match" $
        (testDirectory ++ heightWidthMResultFile) `filesShouldMatch` (testDirectory ++ heightWidthMTestFile)

    context "when the constraints are alignment constraints" $ do
      it "The printed header should match for view 1" $
        (testDirectory ++ align1HeaderResultFile) `filesShouldMatch` (testDirectory ++ align1HeaderTestFile)

      it "The printed implementation should match for view 1" $
        (testDirectory ++ align1MResultFile) `filesShouldMatch` (testDirectory ++ align1MTestFile)

      it "The printed header should match for view 2" $
        (testDirectory ++ align2HeaderResultFile) `filesShouldMatch` (testDirectory ++ align2HeaderTestFile)

      it "The printed implementation should match for view 2" $
        (testDirectory ++ align2MResultFile) `filesShouldMatch` (testDirectory ++ align2MTestFile)

    context "when the constraints are alignment constraints" $ do
      it "The printed header should match" $
        (testDirectory ++ placementHeaderResultFile) `filesShouldMatch` (testDirectory ++ placementHeaderTestFile)

      it "The printed implementation should match" $
        (testDirectory ++ placementMResultFile) `filesShouldMatch` (testDirectory ++ placementMTestFile)

    context "when the constraints are centering constraints" $ do
      it "The printed header should match" $
        (testDirectory ++ centerHeaderResultFile) `filesShouldMatch` (testDirectory ++ centerHeaderTestFile)

      it "The printed implementation should match" $
        (testDirectory ++ centerMResultFile) `filesShouldMatch` (testDirectory ++ centerMTestFile)

sampleAppInfo :: OWAAppInfo
sampleAppInfo = OWAAppInfo {
  appName = "MySampleApp",
  appPrefix = "MSA",
  authorName = "James Bowen",
  dateCreatedString = "4/30/2016",
  companyName = Just "One Week Apps"
}

testFileStructures :: [ObjcFile]
testFileStructures = [objcHeaderFromView sampleAppInfo nameTest1,
  objcImplementationFromView sampleAppInfo nameTest1,
  objcHeaderFromView sampleAppInfo nameTest3,
  objcImplementationFromView sampleAppInfo nameTest3,
  objcHeaderFromView sampleAppInfo elementTest1,
  objcImplementationFromView sampleAppInfo elementTest1,
  objcHeaderFromView sampleAppInfo elementTest2,
  objcImplementationFromView sampleAppInfo elementTest2,
  objcHeaderFromView sampleAppInfo elementTest3,
  objcImplementationFromView sampleAppInfo elementTest3,
  objcHeaderFromView sampleAppInfo heightWidthTestView,
  objcImplementationFromView sampleAppInfo heightWidthTestView,
  objcHeaderFromView sampleAppInfo alignTestView1,
  objcImplementationFromView sampleAppInfo alignTestView1,
  objcHeaderFromView sampleAppInfo alignTestView2,
  objcImplementationFromView sampleAppInfo alignTestView2,
  objcHeaderFromView sampleAppInfo placementTestView,
  objcImplementationFromView sampleAppInfo placementTestView,
  objcHeaderFromView sampleAppInfo centerTestView,
  objcImplementationFromView sampleAppInfo centerTestView]

resultsFiles :: [String]
resultsFiles = [name1HeaderResultFile,
  name1MResultFile,
  name3HeaderResultFile,
  name3MResultFile,
  element1HeaderResultFile,
  element1MResultFile,
  element2HeaderResultFile,
  element2MResultFile,
  element3HeaderResultFile,
  element3MResultFile,
  heightWidthHeaderResultFile,
  heightWidthMResultFile,
  align1HeaderResultFile,
  align1MResultFile,
  align2HeaderResultFile,
  align2MResultFile,
  placementHeaderResultFile,
  placementMResultFile,
  centerHeaderResultFile,
  centerMResultFile]

name1HeaderResultFile :: String
name1HeaderResultFile = "VIANameTestView.h"

name1MResultFile :: String
name1MResultFile = "VIANameTestView.m"

name3HeaderResultFile :: String
name3HeaderResultFile = "VIAPrimaryView.h"

name3MResultFile :: String
name3MResultFile = "VIAPrimaryView.m"

element1HeaderResultFile :: String
element1HeaderResultFile = "VIAElementTest1.h"

element1MResultFile :: String
element1MResultFile = "VIAElementTest1.m"

element2HeaderResultFile :: String
element2HeaderResultFile = "VIAElementTest2.h"

element2MResultFile :: String
element2MResultFile = "VIAElementTest2.m"

element3HeaderResultFile :: String
element3HeaderResultFile = "VIAElementTest3.h"

element3MResultFile :: String
element3MResultFile = "VIAElementTest3.m"

heightWidthHeaderResultFile :: String
heightWidthHeaderResultFile = "VIAConstraintTest2.h"

heightWidthMResultFile :: String
heightWidthMResultFile = "VIAConstraintTest2.m"

align1HeaderResultFile :: String
align1HeaderResultFile = "VIAConstraintTest3.h"

align1MResultFile :: String
align1MResultFile = "VIAConstraintTest3.m"

align2HeaderResultFile :: String
align2HeaderResultFile = "VIAConstraintTest4.h"

align2MResultFile :: String
align2MResultFile = "VIAConstraintTest4.m"

placementHeaderResultFile :: String
placementHeaderResultFile = "VIAConstraintTest5.h"

placementMResultFile :: String
placementMResultFile = "VIAConstraintTest5.m"

centerHeaderResultFile :: String
centerHeaderResultFile = "VIAConstraintTest6.h"

centerMResultFile :: String
centerMResultFile = "VIAConstraintTest6.m"

name1HeaderTestFile :: String
name1HeaderTestFile = "VIANameTestView.h.test"

name1MTestFile :: String
name1MTestFile = "VIANameTestView.m.test"

name3HeaderTestFile :: String
name3HeaderTestFile = "VIAPrimaryView.h.test"

name3MTestFile :: String
name3MTestFile = "VIAPrimaryView.m.test"

element1HeaderTestFile :: String
element1HeaderTestFile = "VIAElementTest1.h.test"

element1MTestFile :: String
element1MTestFile = "VIAElementTest1.m.test"

element2HeaderTestFile :: String
element2HeaderTestFile = "VIAElementTest2.h.test"

element2MTestFile :: String
element2MTestFile = "VIAElementTest2.m.test"

element3HeaderTestFile :: String
element3HeaderTestFile = "VIAElementTest3.h.test"

element3MTestFile :: String
element3MTestFile = "VIAElementTest3.m.test"

heightWidthHeaderTestFile :: String
heightWidthHeaderTestFile = "VIAConstraintTest2.h.test"

heightWidthMTestFile :: String
heightWidthMTestFile = "VIAConstraintTest2.m.test"

align1HeaderTestFile :: String
align1HeaderTestFile = "VIAConstraintTest3.h.test"

align1MTestFile :: String
align1MTestFile = "VIAConstraintTest3.m.test"

align2HeaderTestFile :: String
align2HeaderTestFile = "VIAConstraintTest4.h.test"

align2MTestFile :: String
align2MTestFile = "VIAConstraintTest4.m.test"

placementHeaderTestFile :: String
placementHeaderTestFile = "VIAConstraintTest5.h.test"

placementMTestFile :: String
placementMTestFile = "VIAConstraintTest5.m.test"

centerHeaderTestFile :: String
centerHeaderTestFile = "VIAConstraintTest6.h.test"

centerMTestFile :: String
centerMTestFile = "VIAConstraintTest6.m.test"
