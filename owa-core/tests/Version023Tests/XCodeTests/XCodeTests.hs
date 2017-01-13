-- Core.XCode will expose the method:
-- printBaseXCodeFiles :: FilePath -> OWAAppInfo -> IO ()
-- which takes a directory and app info and will print
-- the requisite files for the basic XCode project to the
-- proper directory. 

module XCodeTests (
  runXCodeTests
) where

import Test.Hspec

import Model.OWAAppInfo
import Core.XCode
import TestUtil

runXCodeTests :: FilePath -> IO ()
runXCodeTests currentDirectory = do
  let testDirectory = currentDirectory ++ testDirectoryExtension
  let fullDffDirectories = map (testDirectory ++) diffFileHomes
  hspec $
    beforeAll_ (mapM_ removeDiffFiles fullDffDirectories)
    . afterAll_ (removeResultsFiles testDirectory producedFiles) $ do
      xcodeTest1 testDirectory
      xcodeTest2 testDirectory

xcodeTest1 :: FilePath -> Spec
xcodeTest1 testDirectory = beforeAll_ (printBaseXCodeFiles testDirectory appInfo1) $ do
  let fullInfo1 = testDirectory ++ info1
  let fullInfoTest = testDirectory ++ infoTest
  let fullAppDelegate1 = testDirectory ++ appDelegate1
  let fullAppDelegate1Test = testDirectory ++ appDelegate1Test
  let fullVC1 = testDirectory ++ vc1
  let fullVC1Test = testDirectory ++ vc1Test
  let fullPBX1 = testDirectory ++ pbx1
  let fullPBX1Test = testDirectory ++ pbx1Test
  let fullContents1 = testDirectory ++ contents1
  let fullContents1Test = testDirectory ++ contents1Test
  describe "Printed XCode Test 1" $ do
    it "Printed info.plist should match (case 1)" $
      fullInfo1 `filesShouldMatch` fullInfoTest

    it "Printed app delegate should match (case 1)" $
      fullAppDelegate1 `filesShouldMatch` fullAppDelegate1Test

    it "Printed view controller should match (case 1)" $
      fullVC1 `filesShouldMatch` fullVC1Test

    it "Printed pbxproj should match (case 1)" $
      fullPBX1 `filesShouldMatch` fullPBX1Test
    
    it "Prints contents should match (case 1)" $
      fullContents1 `filesShouldMatch` fullContents1Test

xcodeTest2 :: FilePath -> Spec
xcodeTest2 testDirectory = beforeAll_ (printBaseXCodeFiles testDirectory appInfo2) $ do
  let fullInfo2 = testDirectory ++ info2
  let fullInfoTest = testDirectory ++ infoTest
  let fullAppDelegate2 = testDirectory ++ appDelegate2
  let fullAppDelegate2Test = testDirectory ++ appDelegate2Test
  let fullVC2 = testDirectory ++ vc2
  let fullVC2Test = testDirectory ++ vc2Test
  let fullPBX2 = testDirectory ++ pbx2
  let fullPBX2Test = testDirectory ++ pbx2Test
  let fullContents2 = testDirectory ++ contents2
  let fullContents2Test = testDirectory ++ contents2Test
  describe "Printed XCode Test 2" $ do
    it "Printed info.plist should match (case 2)" $
      fullInfo2 `filesShouldMatch` fullInfoTest

    it "Printed app delegate should match (case 2)" $
      fullAppDelegate2 `filesShouldMatch` fullAppDelegate2Test

    it "Printed view controller should match (case 2)" $
      fullVC2 `filesShouldMatch` fullVC2Test

    it "Printed pbxproj should match (case 2)" $
      fullPBX2 `filesShouldMatch` fullPBX2Test
    
    it "Prints contents should match (case 2)" $
      fullContents2 `filesShouldMatch` fullContents2Test

appInfo1 :: OWAAppInfo
appInfo1 = OWAAppInfo 
  { appName = "XCodeTestProject"
  , appPrefix = "XCT"
  , authorName = "James Bowen"
  , dateCreatedString = "4/30/2016"
  , companyName = Just "One Week Apps" }
  
appInfo2 :: OWAAppInfo
appInfo2 = OWAAppInfo
  { appName = "MyTestProject"
  , appPrefix = "MTP"
  , authorName = "James Bowen"
  , dateCreatedString = "4/30/2016"
  , companyName = Just "Testing Apps" }

producedFiles :: [FilePath]
producedFiles = 
  [ info1
  , appDelegate1
  , vc1
  , pbx1
  , contents1
  , info2
  , appDelegate2
  , vc2
  , pbx2
  , contents2 ]

testDirectoryExtension :: FilePath
testDirectoryExtension = "/tests/Version023Tests/XCodeTests/"

-- All directories which may contain diffs
diffFileHomes :: [FilePath]
diffFileHomes =
  [ "ios/XCodeTestProject/"
  , "ios/MyTestProject/"
  , "ios/XCodeTestProject.pbxproj/"
  , "ios/MyTestProject.pbxproj/"
  , "ios/XCodeTestProject.pbxproj/.xcworkspacedata/"
  , "ios/MyTestProject.pbxproj/.xcworkspacedata/" ]

infoTest :: FilePath
infoTest = "/OutputFiles/Info.plist.test"

appDelegate1Test :: FilePath
appDelegate1Test = "/OutputFiles/AppDelegate.swift.test"

vc1Test :: FilePath
vc1Test = "/OutputFiles/ViewController.swift.test"

pbx1Test :: FilePath
pbx1Test = "/OutputFiles/project.pbxproj.test"

contents1Test :: FilePath
contents1Test = "/OutputFiles/contents.xcworkspacedata.test"

appDelegate2Test :: FilePath
appDelegate2Test = "/OutputFiles/AppDelegate2.swift.test"

vc2Test :: FilePath
vc2Test = "/OutputFiles/ViewController2.swift.test"

pbx2Test :: FilePath
pbx2Test = "/OutputFiles/project2.pbxproj.test"

contents2Test :: FilePath
contents2Test = "/OutputFiles/contents2.xcworkspacedata.test"

info1 :: FilePath
info1 = "ios/XCodeTestProject/Info.plist"

appDelegate1 :: FilePath
appDelegate1 = "ios/XCodeTestProject/AppDelegate.swift"

vc1 :: FilePath
vc1 = "ios/XCodeTestProject/ViewController.swift"

pbx1 :: FilePath
pbx1 = "ios/XCodeTestProject.xcodeproj/project.pbxproject"

contents1 :: FilePath
contents1 = "ios/XCodeTestProject.xcodeproj/.xcworkspace/contents.xcworkspacedata"

info2 :: FilePath
info2 = "ios/MyTestProject/Info.plist"

appDelegate2 :: FilePath
appDelegate2 = "ios/MyTestProject/AppDelegate.swift"

vc2 :: FilePath
vc2 = "ios/MyTestProject/ViewController.swift"

pbx2 :: FilePath
pbx2 = "ios/MyTestProject.xcodeproj/project.pbxproject"

contents2 :: FilePath
contents2 = "ios/MyTestProject.xcodeproj/.xcworkspace/contents.xcworkspacedata"
