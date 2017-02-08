-- Swift.ViewConverter will expose the method:
-- swiftFileFromView :: OWAAppInfo -> OWAView -> SwiftFile
-- which take an appInfo object and a view and return a
-- file structure of Swift statements
--
-- Swift.Print will expose the method
-- printStructureToFile :: SwiftFile -> FilePath -> IO ()
-- which takes a Swift file structure and a filepath and
-- prints the file structure to the given file
--
-- These tests will first create the file structures and then
-- print them, testing the printed files.

module Swift.Tests.Views.Basic (
  runSwiftViewPrintTests
) where

import Test.Hspec

import Model.OWAAppInfo
import Swift.AbSyn
import Swift.Tests.Utils
import Swift.Tests.Views.Objects
import Swift.Tests.Views.SpecialObjects
import Swift.ViewConverter

runSwiftViewPrintTests :: FilePath -> IO ()
runSwiftViewPrintTests currentDirectory = do
  let testDirectory = currentDirectory ++ dirExtension
  hspec $
    beforeAll_ (removeDiffFiles testDirectory) $
    beforeAll_ (createSwiftResultsFiles testDirectory resultsFiles testFileStructures) 
    . afterAll_ (removeResultsFiles testDirectory resultsFiles) $ do
      elementTests testDirectory
      constraintTests testDirectory
      customViewTests testDirectory
      containerViewTests testDirectory
      scrollViewTests testDirectory

elementTests :: FilePath -> Spec
elementTests testDirectory = describe "Print File Structure for Swift views with sub-elements" $ do
  it "The printed file should match for view 1" $
    (testDirectory ++ element1ResultFile) `filesShouldMatch`
      (testDirectory ++ element1TestFile)
  it "The printed file should match for view 2" $
    (testDirectory ++ element2ResultFile) `filesShouldMatch`
      (testDirectory ++ element2TestFile)
  it "The printed file should match for view 3" $
    (testDirectory ++ element3ResultFile) `filesShouldMatch`
      (testDirectory ++ element3TestFile)

constraintTests :: FilePath -> Spec
constraintTests testDirectory = describe "Print File Structure for Swift views with constraints" $ do
  it "The printed file should match for the view with height/width constraints" $
    (testDirectory ++ heightWidthResultFile) `filesShouldMatch`
      (testDirectory ++ heightWidthTestFile)

  it "The printed file should match for the first view with alignment constraints" $
    (testDirectory ++ align1ResultFile) `filesShouldMatch`
      (testDirectory ++ align1TestFile)

  it "The printed file should match for the second view with alignment constraints" $
    (testDirectory ++ align2ResultFile) `filesShouldMatch`
      (testDirectory ++ align2TestFile)

  it "The printed file should match for the view with placement constraints" $
    (testDirectory ++ placementResultFile) `filesShouldMatch`
      (testDirectory ++ placementTestFile)

  it "The printed file should match for the view with center constraints" $
    (testDirectory ++ centerResultFile) `filesShouldMatch`
      (testDirectory ++ centerTestFile)

customViewTests :: FilePath -> Spec
customViewTests testDirectory = describe "Print File Structure for Swift views with custom views" $ do
  it "The printed file should match for a basic custom view" $
    (testDirectory ++ custom1ResultFile) `filesShouldMatch`
      (testDirectory ++ custom1TestFile)

  it "The printed file should match for a view with two of the same custom views" $
    (testDirectory ++ custom2ResultFile) `filesShouldMatch`
      (testDirectory ++ custom2TestFile)

  it "The printed file should match for a view with two different custom views" $
    (testDirectory ++ custom3ResultFile) `filesShouldMatch`
      (testDirectory ++ custom3TestFile)

containerViewTests :: FilePath -> Spec
containerViewTests testDirectory = describe "Print File Structure for Swift views with container views" $ do
  it "The printed file should match for a basic container view" $
    (testDirectory ++ container1ResultFile) `filesShouldMatch`
      (testDirectory ++ container1TestFile)

  it "The printed file should match for a view with nested container views" $
    (testDirectory ++ container2ResultFile) `filesShouldMatch`
      (testDirectory ++ container2TestFile)

  it "The printed file should match for a view with two different container views" $
    (testDirectory ++ container3ResultFile) `filesShouldMatch`
      (testDirectory ++ container3TestFile)

scrollViewTests :: FilePath -> Spec
scrollViewTests testDirectory = describe "Print File Structure for Swift views with scroll views" $ do
  it "The printed file should match for a default scroll view" $
    (testDirectory ++ scroll1ResultFile) `filesShouldMatch`
      (testDirectory ++ scroll1TestFile)

  it "The printed file should match for a view with a vertical scroll view" $
    (testDirectory ++ scroll2ResultFile) `filesShouldMatch`
      (testDirectory ++ scroll2TestFile)

  it "The printed file should match for a view with a horizontal scroll view" $
    (testDirectory ++ scroll3ResultFile) `filesShouldMatch`
      (testDirectory ++ scroll3TestFile)

dirExtension :: FilePath
dirExtension = "/test/Swift/Tests/Views/OutputFiles/"

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
  [ swiftFileFromView sampleAppInfo elementTest1
  , swiftFileFromView sampleAppInfo elementTest2
  , swiftFileFromView sampleAppInfo elementTest3
  , swiftFileFromView sampleAppInfo heightWidthTestView
  , swiftFileFromView sampleAppInfo align1TestView
  , swiftFileFromView sampleAppInfo align2TestView
  , swiftFileFromView sampleAppInfo placementTestView
  , swiftFileFromView sampleAppInfo centerTestView
  , swiftFileFromView sampleAppInfo basicCustomTest
  , swiftFileFromView sampleAppInfo twoSameCustomTest
  , swiftFileFromView sampleAppInfo twoDifferentCustomTest
  , swiftFileFromView sampleAppInfo basicContainerTest
  , swiftFileFromView sampleAppInfo nestedContainerTest
  , swiftFileFromView sampleAppInfo twoContainersTest
  , swiftFileFromView sampleAppInfo scrollViewVerticalTestView
  , swiftFileFromView sampleAppInfo scrollViewHorizontalTestView 
  , swiftFileFromView sampleAppInfo scrollViewBothTestView ]

resultsFiles :: [FilePath]
resultsFiles =
  [ element1ResultFile
  , element2ResultFile
  , element3ResultFile
  , heightWidthResultFile
  , align1ResultFile
  , align2ResultFile
  , placementResultFile
  , centerResultFile
  , custom1ResultFile
  , custom2ResultFile
  , custom3ResultFile
  , container1ResultFile
  , container2ResultFile
  , container3ResultFile
  , scroll1ResultFile
  , scroll2ResultFile
  , scroll3ResultFile ]

element1ResultFile :: FilePath
element1ResultFile = "VIAElementTest1.swift"

element2ResultFile :: FilePath
element2ResultFile = "VIAElementTest2.swift"

element3ResultFile :: FilePath
element3ResultFile = "VIAElementTest3.swift"

heightWidthResultFile :: FilePath
heightWidthResultFile = "VIAConstraintTest2.swift"

align1ResultFile :: FilePath
align1ResultFile = "VIAConstraintTest3.swift"

align2ResultFile :: FilePath
align2ResultFile = "VIAConstraintTest4.swift"

placementResultFile :: FilePath
placementResultFile = "VIAConstraintTest5.swift"

centerResultFile :: FilePath
centerResultFile = "VIAConstraintTest6.swift"

custom1ResultFile :: FilePath
custom1ResultFile = "IGACustomTest1.swift"

custom2ResultFile :: FilePath
custom2ResultFile = "IGACustomTest2.swift"

custom3ResultFile :: FilePath
custom3ResultFile = "IGACustomTest3.swift"

container1ResultFile :: FilePath
container1ResultFile = "IGAContainerView1.swift"

container2ResultFile :: FilePath
container2ResultFile = "IGAContainerView2.swift"

container3ResultFile :: FilePath
container3ResultFile = "IGAContainerView3.swift"

scroll1ResultFile :: FilePath
scroll1ResultFile = "IGAScrollView1.swift"

scroll2ResultFile :: FilePath
scroll2ResultFile = "IGAScrollView2.swift"

scroll3ResultFile :: FilePath
scroll3ResultFile = "IGAScrollView3.swift"

element1TestFile :: FilePath
element1TestFile = "VIAElementTest1.swift.test"

element2TestFile :: FilePath
element2TestFile = "VIAElementTest2.swift.test"

element3TestFile :: FilePath
element3TestFile = "VIAElementTest3.swift.test"

heightWidthTestFile :: FilePath
heightWidthTestFile = "VIAConstraintTest2.swift.test"

align1TestFile :: FilePath
align1TestFile = "VIAConstraintTest3.swift.test"

align2TestFile :: FilePath
align2TestFile = "VIAConstraintTest4.swift.test"

placementTestFile :: FilePath
placementTestFile = "VIAConstraintTest5.swift.test"

centerTestFile :: FilePath
centerTestFile = "VIAConstraintTest6.swift.test"

custom1TestFile :: FilePath
custom1TestFile = "IGACustomTest1.swift.test"

custom2TestFile :: FilePath
custom2TestFile = "IGACustomTest2.swift.test"

custom3TestFile :: FilePath
custom3TestFile = "IGACustomTest3.swift.test"

container1TestFile :: FilePath
container1TestFile = "IGAContainerView1.swift.test"

container2TestFile :: FilePath
container2TestFile = "IGAContainerView2.swift.test"

container3TestFile :: FilePath
container3TestFile = "IGAContainerView3.swift.test"

scroll1TestFile :: FilePath
scroll1TestFile = "IGAScrollView1.swift.test"

scroll2TestFile :: FilePath
scroll2TestFile = "IGAScrollView2.swift.test"

scroll3TestFile :: FilePath
scroll3TestFile = "IGAScrollView3.swift.test"
