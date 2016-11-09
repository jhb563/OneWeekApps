-- OWAXCode will expose the method:
-- printBaseXCodeFiles :: FilePath -> OWAAppInfo -> IO ()
-- which takes a directory and app info and will print
-- the requisite files for the basic XCode project to the
-- proper directory. 

module XCodeTests (
  runXCodeTests
) where

import OWAAppInfo
import OWAXCode
import Test.Hspec
import TestUtil

runXCodeTests :: FilePath -> IO ()
runXCodeTests currentDirectory = do
  let testDirectory = currentDirectory ++ testDirectoryExtension
  let project1Directory = currentDirectory ++ project1Extension
  let project2Directory = currentDirectory ++ project2Extension
  hspec $
    beforeAll_ (removeDiffFiles project1Directory) $
    beforeAll_ (removeDiffFiles project2Directory)
    . afterAll_ (removeResultsFiles testDirectory producedFiles) $ do
      xcodeTest1 testDirectory
      xcodeTest2 testDirectory

xcodeTest1 :: FilePath -> Spec
xcodeTest1 testDirectory = beforeAll_ (printBaseXCodeFiles testDirectory appInfo1) $
  describe "Printed XCode Test 1" $ do
    it "Printed info.plist should match (case 1)" $
      info1 `filesShouldMatch` infoTest

    it "Printed app delegate should match (case 1)" $
      appDelegate1 `filesShouldMatch` appDelegate1Test

    it "Printed view controller should match (case 1)" $
      vc1 `filesShouldMatch` vc1Test

    it "Printed pbxproj should match (case 1)" $
      pbx1 `filesShouldMatch` pbx1Test
    
    it "Prints contents should match (case 1)" $
      contents1 `filesShouldMatch` contents1Test

xcodeTest2 :: FilePath -> Spec
xcodeTest2 testDirectory = beforeAll_ (printBaseXCodeFiles testDirectory appInfo2) $
  describe "Printed XCode Test 2" $ do
    it "Printed info.plist should match (case 2)" $
      info2 `filesShouldMatch` infoTest

    it "Printed app delegate should match (case 2)" $
      appDelegate2 `filesShouldMatch` appDelegate2Test

    it "Printed view controller should match (case 2)" $
      vc2 `filesShouldMatch` vc2Test

    it "Printed pbxproj should match (case 2)" $
      pbx2 `filesShouldMatch` pbx2Test
    
    it "Prints contents should match (case 2)" $
      contents2 `filesShouldMatch` contents2Test

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

project1Extension :: FilePath
project1Extension = "/ios/XCodeTestProject/"

project2Extension :: FilePath
project2Extension = "/ios/XCodeTestProject/"

infoTest :: FilePath
infoTest = "/OutputFiles/Info.plist.test"

appDelegate1Test :: FilePath
appDelegate1Test = "/OutputFiles/AppDelegate.swift.test"

vc1Test :: FilePath
vc1Test = "/OutputFiles/ViewController.swift.test"

pbx1Test :: FilePath
pbx1Test = "/OutputFiles/project.pbxproject.test"

contents1Test :: FilePath
contents1Test = "/OutputFiles/contents.xcworkspacedata.test"

appDelegate2Test :: FilePath
appDelegate2Test = "/OutputFiles/AppDelegate2.swift.test"

vc2Test :: FilePath
vc2Test = "/OutputFiles/ViewController2.swift.test"

pbx2Test :: FilePath
pbx2Test = "/OutputFiles/project.pbxproject.test"

contents2Test :: FilePath
contents2Test = "/OutputFiles/contents.xcworkspacedata.test"

info1 :: FilePath
info1 = "/ios/XCodeTestProject/Info.plist.test"

appDelegate1 :: FilePath
appDelegate1 = "/ios/XCodeTestProject/AppDelegate.swift.test"

vc1 :: FilePath
vc1 = "/ios/XCodeTestProject/ViewController.swift.test"

pbx1 :: FilePath
pbx1 = "/ios/XCodeTestProject/project.pbxproject.test"

contents1 :: FilePath
contents1 = "/ios/XCodeTestProject/contents.xcworkspacedata.test"

info2 :: FilePath
info2 = "/ios/MyTestProject/Info.plist.test"

appDelegate2 :: FilePath
appDelegate2 = "/ios/MyTestProject/AppDelegate.swift.test"

vc2 :: FilePath
vc2 = "/ios/MyTestProject/ViewController.swift.test"

pbx2 :: FilePath
pbx2 = "/ios/MyTestProject/project.pbxproject.test"

contents2 :: FilePath
contents2 = "/ios/MyTestProject/contents.xcworkspacedata.test"
