-- Objc.XCode will expose the methods
-- initialVCHeader :: OWAAppInfo -> ObjcFile
-- initialVCImplementation :: OWAAppInfo -> ObjcFile
-- appDelegateHeader :: OWAAppInfo -> ObjcFile
-- appDelegateImplementation :: OWAAppInfo -> ObjcFile
-- mainFileM :: OWAAppInfo -> ObjcFile
-- which each take an appInfo object and a list of alerts and return a
-- file structure of objective C statements
--
-- Objc.Print will expose the methods
-- printStructureToFile :: ObjcFile -> FilePath -> IO ()
-- which takes an objective C file structure and a filepath
-- and prints the file structure to the given file
--
-- These tests will first create the file structures and then
-- print them, testing the printed files.

module Objc.Tests.XCode.Basic (
  runObjcXCodeTests
) where

import Test.Hspec

import Model.OWAAppInfo
import Objc.AbSyn
import Objc.Tests.Utils (removeDiffFiles, removeResultsFiles, createResultsFiles, filesShouldMatch)
import Objc.XCode

runObjcXCodeTests :: FilePath -> IO ()
runObjcXCodeTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/test/Objc/Tests/XCode/OutputFiles/"
  hspec $
    beforeAll_ (removeDiffFiles testDirectory) $
    beforeAll_ (createResultsFiles testDirectory resultsFiles testFileStructures)
    . afterAll_ (removeResultsFiles testDirectory resultsFiles) $ do
      xcodeTest1 testDirectory
      xcodeTest2 testDirectory

xcodeTest1 :: FilePath -> Spec
xcodeTest1 testDirectory = describe "Print XCode File Structures for app info 1" $ do
  it "The app delegate header should match" $
    (testDirectory ++ appDelegate1HeaderResult) `filesShouldMatch`
      (testDirectory ++ appDelegate1HeaderTest)

  it "The app delegate implementation should match" $
    (testDirectory ++ appDelegate1MResult) `filesShouldMatch`
      (testDirectory ++ appDelegate1MTest)

  it "The view controller header should match" $
    (testDirectory ++ viewController1HeaderResult) `filesShouldMatch`
      (testDirectory ++ viewController1HeaderTest)

  it "The view controller implementation should match" $
    (testDirectory ++ viewController1MResult) `filesShouldMatch`
      (testDirectory ++ viewController1MTest)

  it "The main file should match" $
    (testDirectory ++ main1Result) `filesShouldMatch`
      (testDirectory ++ main1Test)

xcodeTest2 :: FilePath -> Spec
xcodeTest2 testDirectory = describe "Print XCode File Structures for app info 2" $ do
  it "The app delegate header should match" $
    (testDirectory ++ appDelegate2HeaderResult) `filesShouldMatch`
      (testDirectory ++ appDelegate2HeaderTest)

  it "The app delegate implementation should match" $
    (testDirectory ++ appDelegate2MResult) `filesShouldMatch`
      (testDirectory ++ appDelegate2MTest)

  it "The view controller header should match" $
    (testDirectory ++ viewController2HeaderResult) `filesShouldMatch`
      (testDirectory ++ viewController2HeaderTest)

  it "The view controller implementation should match" $
    (testDirectory ++ viewController2MResult) `filesShouldMatch`
      (testDirectory ++ viewController2MTest)

  it "The main file should match" $
    (testDirectory ++ main2Result) `filesShouldMatch`
      (testDirectory ++ main2Test)

appInfo1 :: OWAAppInfo
appInfo1 = OWAAppInfo
  { appName = "IntegrationApp"
  , appPrefix = "IGA"
  , authorName = "James"
  , dateCreatedString = "04/16/2017"
  , companyName = Just "One Week Apps" }

appInfo2 :: OWAAppInfo
appInfo2 = OWAAppInfo
  { appName = "Second Test App"
  , appPrefix = "STA"
  , authorName = "John"
  , dateCreatedString = "03/15/2017" 
  , companyName = Nothing }

testFileStructures :: [ObjcFile]
testFileStructures = 
  [ appDelegateHeader appInfo1
  , appDelegateImplementation appInfo1
  , initialVCHeader appInfo1
  , initialVCImplementation appInfo1
  , mainFileM appInfo1
  , appDelegateHeader appInfo2
  , appDelegateImplementation appInfo2
  , initialVCHeader appInfo2
  , initialVCImplementation appInfo2
  , mainFileM appInfo2
  ]

resultsFiles :: [FilePath]
resultsFiles = 
  [ appDelegate1HeaderResult
  , appDelegate1MResult
  , viewController1HeaderResult
  , viewController1MResult
  , main1Result
  , appDelegate2HeaderResult
  , appDelegate2MResult
  , viewController2HeaderResult
  , viewController2MResult
  , main2Result
  ]

appDelegate1HeaderResult :: FilePath
appDelegate1HeaderResult = "AppDelegate1.h"

appDelegate1MResult :: FilePath
appDelegate1MResult = "AppDelegate1.m"

viewController1HeaderResult :: FilePath
viewController1HeaderResult = "ViewController1.h"

viewController1MResult :: FilePath
viewController1MResult = "ViewController1.m"

appDelegate2HeaderResult :: FilePath
appDelegate2HeaderResult = "AppDelegate2.h"

appDelegate2MResult :: FilePath
appDelegate2MResult = "AppDelegate2.m"

viewController2HeaderResult :: FilePath
viewController2HeaderResult = "ViewController2.h"

viewController2MResult :: FilePath
viewController2MResult = "ViewController2.m"

main1Result :: FilePath
main1Result = "main1.m"

main2Result :: FilePath
main2Result = "main2.m"

appDelegate1HeaderTest :: FilePath
appDelegate1HeaderTest = "AppDelegate1.h.test"

appDelegate1MTest :: FilePath
appDelegate1MTest = "AppDelegate1.m.test"

viewController1HeaderTest :: FilePath
viewController1HeaderTest = "ViewController1.h.test"

viewController1MTest :: FilePath
viewController1MTest = "ViewController1.m.test"

appDelegate2HeaderTest :: FilePath
appDelegate2HeaderTest = "AppDelegate2.h.test"

appDelegate2MTest :: FilePath
appDelegate2MTest = "AppDelegate2.m.test"

viewController2HeaderTest :: FilePath
viewController2HeaderTest = "ViewController2.h.test"

viewController2MTest :: FilePath
viewController2MTest = "ViewController2.m.test"

main1Test :: FilePath
main1Test = "main1.m.test"

main2Test :: FilePath
main2Test = "main2.m.test"
