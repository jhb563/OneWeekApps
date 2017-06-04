-- Core.XCode will expose the method:
-- printBaseXCodeFiles :: FilePath -> OWAAppInfo -> IO ()
-- which takes a directory and app info and will print
-- the requisite files for the basic XCode project to the
-- proper directory. 

module Core.Tests.XCode.Basic (
  runXCodeTests
) where

import Test.Hspec

import Model.OWAAppInfo
import Core.Tests.Utils
import Core.XCode

runXCodeTests :: FilePath -> IO ()
runXCodeTests currentDirectory = do
  let testDirectory = currentDirectory ++ testDirectoryExtension
  let fullDiffDirectories = map (testDirectory ++) diffFileHomes
  hspec $
    beforeAll_ (mapM_ removeDiffFiles fullDiffDirectories)
    . afterAll_ (removeResultsFiles testDirectory producedFiles) $ do
      xcodeTest1 testDirectory
      xcodeTest2 testDirectory

xcodeTest1 :: FilePath -> Spec
xcodeTest1 testDirectory = beforeAll_ (printBaseXCodeFiles testDirectory appInfo1) $ do
  -- Shared Test Files
  let fullInfoTest = testDirectory ++ infoTest
  let fullContents1Test = testDirectory ++ contents1Test
  -- Swift Only Test Files
  let swiftFullAppDelegate1Test = testDirectory ++ appDelegateSwift1Test
  let swiftFullVC1Test = testDirectory ++ vcSwift1Test
  let swiftFullPBX1Test = testDirectory ++ pbxSwift1Test
  -- Swift Result Files
  let swiftFullInfo1 = testDirectory ++ infoSwift1
  let swiftFullAppDelegate1 = testDirectory ++ appDelegateSwift1
  let swiftFullVC1 = testDirectory ++ vcSwift1
  let swiftFullPBX1 = testDirectory ++ pbxSwift1
  let swiftFullContents1 = testDirectory ++ contentsSwift1
  -- Objc Only Test Files
  let objcFullAppDelegateHeader1Test = testDirectory ++ appDelegateHeader1Test
  let objcFullAppDelegateM1Test = testDirectory ++ appDelegateM1Test
  let objcFullVCHeader1Test = testDirectory ++ vcHeader1Test
  let objcFullVCM1Test = testDirectory ++ vcM1Test
  let objcFullMain1Test = testDirectory ++ main1Test
  let objcFullPBX1Test = testDirectory ++ pbx1ObjcTest
  -- Objc Result Files
  let objcFullInfo1 = testDirectory ++ infoObjc1
  let objcFullAppDelegateHeader1 = testDirectory ++ appDelegateHeader1
  let objcFullAppDelegateM1 = testDirectory ++ appDelegateM1
  let objcFullVCHeader1 = testDirectory ++ vcHeader1
  let objcFullVCM1 = testDirectory ++ vcM1
  let objcFullMain1 = testDirectory ++ mainObjc1
  let objcFullPBX1 = testDirectory ++ pbxObjc1
  let objcFullContents1 = testDirectory ++ contentsObjc1
  describe "Printed XCode Test 1" $ do
    it "Printed (swift) info.plist should match (case 1)" $
      swiftFullInfo1 `filesShouldMatch` fullInfoTest

    it "Printed (swift) app delegate should match (case 1)" $
      swiftFullAppDelegate1 `filesShouldMatch` swiftFullAppDelegate1Test

    it "Printed (swift) view controller should match (case 1)" $
      swiftFullVC1 `filesShouldMatch` swiftFullVC1Test

    it "Printed (swift) pbxproj should match (case 1)" $
      swiftFullPBX1 `filesShouldMatch` swiftFullPBX1Test
    
    it "Printed (swift) contents should match (case 1)" $
      swiftFullContents1 `filesShouldMatch` fullContents1Test

    it "Printed (objc) info.plist should match (case 1)" $
      objcFullInfo1 `filesShouldMatch` fullInfoTest

    it "Printed (objc) app delegate header should match (case 1)" $
      objcFullAppDelegateHeader1 `filesShouldMatch` objcFullAppDelegateHeader1Test

    it "Printed (objc) app delegate implementation should match (case 1)" $
      objcFullAppDelegateM1 `filesShouldMatch` objcFullAppDelegateM1Test

    it "Printed (objc) view controller header should match (case 1)" $
      objcFullVCHeader1 `filesShouldMatch` objcFullVCHeader1Test

    it "Printed (objc) view controller implementation should match (case 1)" $
      objcFullVCM1 `filesShouldMatch` objcFullVCM1Test

    it "Print (objc) main file should match (case 1)" $
      objcFullMain1 `filesShouldMatch` objcFullMain1Test 

    it "Printed (objc) pbxproj should match (case 1)" $
      objcFullPBX1 `filesShouldMatch` objcFullPBX1Test
    
    it "Printed (objc) contents should match (case 1)" $
      objcFullContents1 `filesShouldMatch` fullContents1Test

xcodeTest2 :: FilePath -> Spec
xcodeTest2 testDirectory = beforeAll_ (printBaseXCodeFiles testDirectory appInfo2) $ do
  -- Shared Test Files
  let fullInfoTest = testDirectory ++ infoTest
  let fullContents2Test = testDirectory ++ contents2Test
  -- Swift Only Test Files
  let swiftFullAppDelegate2Test = testDirectory ++ appDelegateSwift2Test
  let swiftFullVC2Test = testDirectory ++ vcSwift2Test
  let swiftFullPBX2Test = testDirectory ++ pbxSwift2Test
  -- Swift Result Files
  let swiftFullInfo2 = testDirectory ++ infoSwift2
  let swiftFullAppDelegate2 = testDirectory ++ appDelegateSwift2
  let swiftFullVC2 = testDirectory ++ vcSwift2
  let swiftFullPBX2 = testDirectory ++ pbxSwift2
  let swiftFullContents2 = testDirectory ++ contentsSwift2
  -- Objc Only Test Files
  let objcFullAppDelegateHeader2Test = testDirectory ++ appDelegateHeader2Test
  let objcFullAppDelegateM2Test = testDirectory ++ appDelegateM2Test
  let objcFullVCHeader2Test = testDirectory ++ vcHeader2Test
  let objcFullVCM2Test = testDirectory ++ vcM2Test
  let objcFullMain2Test = testDirectory ++ main2Test
  let objcFullPBX2Test = testDirectory ++ pbx2ObjcTest
  -- Objc Result Files
  let objcFullInfo2 = testDirectory ++ infoObjc2
  let objcFullAppDelegateHeader2 = testDirectory ++ appDelegateHeader2
  let objcFullAppDelegateM2 = testDirectory ++ appDelegateM2
  let objcFullVCHeader2 = testDirectory ++ vcHeader2
  let objcFullVCM2 = testDirectory ++ vcM2
  let objcFullMain2 = testDirectory ++ mainObjc2
  let objcFullPBX2 = testDirectory ++ pbxObjc2
  let objcFullContents2 = testDirectory ++ contentsObjc2
  describe "Printed XCode Test 2" $ do
    it "Printed (swift) info.plist should match (case 2)" $
      swiftFullInfo2 `filesShouldMatch` fullInfoTest

    it "Printed (swift) app delegate should match (case 2)" $
      swiftFullAppDelegate2 `filesShouldMatch` swiftFullAppDelegate2Test

    it "Printed (swift) view controller should match (case 2)" $
      swiftFullVC2 `filesShouldMatch` swiftFullVC2Test

    it "Printed (swift) pbxproj should match (case 2)" $
      swiftFullPBX2 `filesShouldMatch` swiftFullPBX2Test
    
    it "Printed (swift) contents should match (case 2)" $
      swiftFullContents2 `filesShouldMatch` fullContents2Test

    it "Printed (objc) info.plist should match (case 2)" $
      objcFullInfo2 `filesShouldMatch` fullInfoTest

    it "Printed (objc) app delegate header should match (case 2)" $
      objcFullAppDelegateHeader2 `filesShouldMatch` objcFullAppDelegateHeader2Test

    it "Printed (objc) app delegate implementation should match (case 2)" $
      objcFullAppDelegateM2 `filesShouldMatch` objcFullAppDelegateM2Test

    it "Printed (objc) view controller header should match (case 2)" $
      objcFullVCHeader2 `filesShouldMatch` objcFullVCHeader2Test

    it "Printed (objc) view controller implementation should match (case 2)" $
      objcFullVCM2 `filesShouldMatch` objcFullVCM2Test

    it "Print (objc) main file should match (case 2)" $
      objcFullMain2 `filesShouldMatch` objcFullMain2Test 

    it "Printed (objc) pbxproj should match (case 2)" $
      objcFullPBX2 `filesShouldMatch` objcFullPBX2Test
    
    it "Printed (objc) contents should match (case 2)" $
      objcFullContents2 `filesShouldMatch` fullContents2Test

appInfo1 :: OWAAppInfo
appInfo1 = OWAAppInfo 
  { appName = "XCodeTestProject"
  , appPrefix = "XCT"
  , authorName = "James Bowen"
  , dateCreatedString = "4/30/2016"
  , companyName = Just "One Week Apps" }
  
appInfo2 :: OWAAppInfo
appInfo2 = OWAAppInfo
  { appName = "My Test Project"
  , appPrefix = "MTP"
  , authorName = "James Bowen"
  , dateCreatedString = "4/30/2016"
  , companyName = Just "Testing Apps" }

producedFiles :: [FilePath]
producedFiles = 
  [ infoSwift1
  , appDelegateSwift1
  , vcSwift1
  , pbxSwift1
  , contentsSwift1
  , infoSwift2
  , appDelegateSwift2
  , vcSwift2
  , pbxSwift2
  , contentsSwift2 
  , infoObjc1
  , appDelegateHeader1
  , appDelegateM1
  , vcHeader1
  , vcM1
  , mainObjc1
  , pbxObjc1
  , contentsObjc1
  , infoObjc2
  , appDelegateHeader2
  , appDelegateM2
  , vcHeader2
  , vcM2
  , mainObjc2
  , pbxObjc2
  , contentsObjc2
  ]

testDirectoryExtension :: FilePath
testDirectoryExtension = "/test/Core/Tests/XCode/"

-- All directories which may contain diffs
diffFileHomes :: [FilePath]
diffFileHomes =
  [ "swift/XCodeTestProject/"
  , "swift/My Test Project/"
  , "swift/XCodeTestProject.pbxproj/"
  , "swift/My Test Project.pbxproj/"
  , "swift/XCodeTestProject.pbxproj/.xcworkspacedata/"
  , "swift/My Test Project.pbxproj/.xcworkspacedata/"
  , "objc/XCodeTestProject/"
  , "objc/My Test Project/"
  , "objc/XCodeTestProject.pbxproj/"
  , "objc/My Test Project.pbxproj/"
  , "objc/XCodeTestProject.pbxproj/.xcworkspacedata/"
  , "objc/My Test Project.pbxproj/.xcworkspacedata/"
  ]

-- Swift Test Files

infoTest :: FilePath
infoTest = "/OutputFiles/Info.plist.test"

appDelegateSwift1Test :: FilePath
appDelegateSwift1Test = "/OutputFiles/AppDelegate.swift.test"

vcSwift1Test :: FilePath
vcSwift1Test = "/OutputFiles/ViewController.swift.test"

pbxSwift1Test :: FilePath
pbxSwift1Test = "/OutputFiles/project.pbxproj.test"

contents1Test :: FilePath
contents1Test = "/OutputFiles/contents.xcworkspacedata.test"

appDelegateSwift2Test :: FilePath
appDelegateSwift2Test = "/OutputFiles/AppDelegate2.swift.test"

vcSwift2Test :: FilePath
vcSwift2Test = "/OutputFiles/ViewController2.swift.test"

pbxSwift2Test :: FilePath
pbxSwift2Test = "/OutputFiles/project2.pbxproj.test"

contents2Test :: FilePath
contents2Test = "/OutputFiles/contents2.xcworkspacedata.test"

-- Swift Result Files

infoSwift1 :: FilePath
infoSwift1 = "swift/XCodeTestProject/Info.plist"

appDelegateSwift1 :: FilePath
appDelegateSwift1 = "swift/XCodeTestProject/AppDelegate.swift"

vcSwift1 :: FilePath
vcSwift1 = "swift/XCodeTestProject/ViewController.swift"

pbxSwift1 :: FilePath
pbxSwift1 = "swift/XCodeTestProject.xcodeproj/project.pbxproj"

contentsSwift1 :: FilePath
contentsSwift1 = "swift/XCodeTestProject.xcodeproj/.xcworkspace/contents.xcworkspacedata"

infoSwift2 :: FilePath
infoSwift2 = "swift/My Test Project/Info.plist"

appDelegateSwift2 :: FilePath
appDelegateSwift2 = "swift/My Test Project/AppDelegate.swift"

vcSwift2 :: FilePath
vcSwift2 = "swift/My Test Project/ViewController.swift"

pbxSwift2 :: FilePath
pbxSwift2 = "swift/My Test Project.xcodeproj/project.pbxproj"

contentsSwift2 :: FilePath
contentsSwift2 = "swift/My Test Project.xcodeproj/.xcworkspace/contents.xcworkspacedata"

-- Objc Test Files

infoObjcTest :: FilePath
infoObjcTest = "/OutputFiles/Info.plist.test"

appDelegateHeader1Test :: FilePath
appDelegateHeader1Test = "/OutputFiles/AppDelegate.h.test"

appDelegateM1Test :: FilePath
appDelegateM1Test = "/OutputFiles/AppDelegate.m.test"

vcHeader1Test :: FilePath
vcHeader1Test = "/OutputFiles/ViewController.h.test"

vcM1Test :: FilePath
vcM1Test = "/OutputFiles/ViewController.m.test"

main1Test :: FilePath
main1Test = "/OutputFiles/main.m.test"

pbx1ObjcTest :: FilePath
pbx1ObjcTest = "/OutputFiles/projectObjc.pbxproj.test"

appDelegateHeader2Test :: FilePath
appDelegateHeader2Test = "/OutputFiles/AppDelegate2.h.test"

appDelegateM2Test :: FilePath
appDelegateM2Test = "/OutputFiles/AppDelegate2.m.test"

vcHeader2Test :: FilePath
vcHeader2Test = "/OutputFiles/ViewController2.h.test"

vcM2Test :: FilePath
vcM2Test = "/OutputFiles/ViewController2.m.test"

main2Test :: FilePath
main2Test = "/OutputFiles/main2.m.test"

pbx2ObjcTest :: FilePath
pbx2ObjcTest = "/OutputFiles/project2Objc.pbxproj.test"

-- Objc Result Files

infoObjc1 :: FilePath
infoObjc1 = "objc/XCodeTestProject/Info.plist"

appDelegateHeader1 :: FilePath
appDelegateHeader1 = "objc/XCodeTestProject/AppDelegate.h"

appDelegateM1 :: FilePath
appDelegateM1 = "objc/XCodeTestProject/AppDelegate.m"

vcHeader1 :: FilePath
vcHeader1 = "objc/XCodeTestProject/ViewController.h"

vcM1 :: FilePath
vcM1 = "objc/XCodeTestProject/ViewController.m"

mainObjc1 :: FilePath
mainObjc1 = "objc/XCodeTestProject/main.m"

pbxObjc1 :: FilePath
pbxObjc1 = "objc/XCodeTestProject.xcodeproj/project.pbxproj"

contentsObjc1 :: FilePath
contentsObjc1 = "objc/XCodeTestProject.xcodeproj/.xcworkspace/contents.xcworkspacedata"

infoObjc2 :: FilePath
infoObjc2 = "objc/My Test Project/Info.plist"

appDelegateHeader2 :: FilePath
appDelegateHeader2 = "objc/My Test Project/AppDelegate.h"

appDelegateM2 :: FilePath
appDelegateM2 = "objc/My Test Project/AppDelegate.m"

vcHeader2 :: FilePath
vcHeader2 = "objc/My Test Project/ViewController.h"

vcM2 :: FilePath
vcM2 = "objc/My Test Project/ViewController.m"

mainObjc2 :: FilePath
mainObjc2 = "objc/My Test Project/main.m"

pbxObjc2 :: FilePath
pbxObjc2 = "objc/My Test Project.xcodeproj/project.pbxproj"

contentsObjc2 :: FilePath
contentsObjc2 = "objc/My Test Project.xcodeproj/.xcworkspace/contents.xcworkspacedata"
