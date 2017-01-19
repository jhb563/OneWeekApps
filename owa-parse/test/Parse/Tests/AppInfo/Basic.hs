-- This module will test the function:
-- findAppInfoFile :: FilePath -> IO (Maybe FilePath)
-- from Core.FileSearch, which takes a directory and returns the path to
-- the app.info file, as well as the function:
-- parseAppInfoFromFile :: FilePath -> IO Either [OWAParseError] OWAAppInfo
-- in the module AppInfoParser which will take the possible 
-- filepath of the app info and returns a structure with the app info, 
-- with default values filled in if there is no filepath.

module Parse.Tests.AppInfo.Basic (
  runAppInfoParseTests
) where

import Test.Hspec
import System.Directory

import Parse.AppInfoParser
import Parse.Tests.AppInfo.Errors
import Parse.Tests.AppInfo.Objects
import Parse.Tests.Utils (shouldReturnLefts, shouldReturnRights, shouldMatchError)

runAppInfoParseTests :: FilePath -> IO ()
runAppInfoParseTests currentDirectory = hspec $ do
  let parseDirectory = currentDirectory ++ "/test/Parse/Tests/AppInfo/ParseFiles"
  appInfoPassTests parseDirectory
  appInfoFailTests parseDirectory

appInfoPassTests :: FilePath -> Spec
appInfoPassTests testDirectory = do
  let testFile1 = testDirectory ++ appInfoPassExtension1
  let testFile2 = testDirectory ++ appInfoPassExtension2
  let testFile3 = testDirectory ++ appInfoPassExtension3
  let testFile4 = testDirectory ++ appInfoPassExtension4
  let testFile5 = testDirectory ++ appInfoPassExtension5
  describe "Parse app info from correctly formatted files" $ do
    context "with a Author, Created date, and Company Name" $
      it "Should return the app info contained in the file" $
        parseAppInfoFromFile testFile1 `shouldReturnRights` appInfo1

    context "with a Author, Created date, and Company Name in a different order" $
      it "Should return the app info contained in the file" $
        parseAppInfoFromFile testFile2 `shouldReturnRights` appInfo2

    context "with no company name" $
      it "Should return the app info contained in the file but with no company" $
        parseAppInfoFromFile testFile3 `shouldReturnRights` appInfo3

    context "with no author name" $
      it "Should return the app info contained in the file but with a blank author name" $
        parseAppInfoFromFile testFile4 `shouldReturnRights` appInfo4

    context "with comments both before and inline" $
      it "Should return the app info contained in the file ignoring the comments" $
        parseAppInfoFromFile testFile5 `shouldReturnRights` appInfo5

appInfoFailTests :: FilePath -> Spec
appInfoFailTests testDirectory = do
  let testFile1 = testDirectory ++ appInfoFailExtension1
  let testFile2 = testDirectory ++ appInfoFailExtension2
  let testFile3 = testDirectory ++ appInfoFailExtension3
  let testFile4 = testDirectory ++ appInfoFailExtension4
  let testFile5 = testDirectory ++ appInfoFailExtension5
  let testFile6 = testDirectory ++ appInfoFailExtension6
  let testFile7 = testDirectory ++ appInfoFailExtension7
  let testFile8 = testDirectory ++ appInfoFailExtension8
  let testFile9 = testDirectory ++ appInfoFailExtension9
  describe "Parse app info from incorrectly formatted files" $ do
    context "when a keyword is not capitalized" $
      it "Should return a parse error highlighting the keyword" $
        parseAppInfoFromFile testFile1 `shouldMatchError` appError1

    context "when a fictional keyword is used" $
      it "Should return a parse error highlighting the keyword" $
        parseAppInfoFromFile testFile2 `shouldMatchError` appError2

    context "when a colon is not used after a keyword" $
      it "Should return a parse error highlighting the keyword" $
        parseAppInfoFromFile testFile3 `shouldMatchError` appError3

    context "when the date is poorly formatted" $
      it "Should return a parse error highlighting the date" $
        parseAppInfoFromFile testFile4 `shouldMatchError` appError4

    context "when the app name is omitted" $
      it "Should return an object error highlighting the need for an app name" $
        parseAppInfoFromFile testFile5 `shouldReturnLefts` [appError5]

    context "when the prefix is omitted" $
      it "Should return an object error highlighting the need for a prefix" $
        parseAppInfoFromFile testFile6 `shouldReturnLefts` [appError6]

    context "when the prefix is too short" $
      it "Should return an object error highlighting the incorrect prefix" $
        parseAppInfoFromFile testFile7 `shouldMatchError` appError7

    context "when the prefix is too long" $
      it "Should return an object error highlighting the incorrect prefix" $
        parseAppInfoFromFile testFile8 `shouldMatchError` appError8

    context "when the prefix is lowercase" $
      it "Should return an object error highlighting the incorrect prefix" $
        parseAppInfoFromFile testFile9 `shouldMatchError` appError9

appInfoPassExtension1 :: String
appInfoPassExtension1 = "/appInfoPassTest1.info"

appInfoPassExtension2 :: String
appInfoPassExtension2 = "/appInfoPassTest2.info"

appInfoPassExtension3 :: String
appInfoPassExtension3 = "/appInfoPassTest3.info"

appInfoPassExtension4 :: String
appInfoPassExtension4 = "/appInfoPassTest4.info"

appInfoPassExtension5 :: String
appInfoPassExtension5 = "/appInfoPassTest5.info"

appInfoFailExtension1 :: String
appInfoFailExtension1 = "/appInfoFailTest1.info"

appInfoFailExtension2 :: String
appInfoFailExtension2 = "/appInfoFailTest2.info"

appInfoFailExtension3 :: String
appInfoFailExtension3 = "/appInfoFailTest3.info"

appInfoFailExtension4 :: String
appInfoFailExtension4 = "/appInfoFailTest4.info"

appInfoFailExtension5 :: String
appInfoFailExtension5 = "/appInfoFailTest5.info"

appInfoFailExtension6 :: String
appInfoFailExtension6 = "/appInfoFailTest6.info"

appInfoFailExtension7 :: String
appInfoFailExtension7 = "/appInfoFailTest7.info"

appInfoFailExtension8 :: String
appInfoFailExtension8 = "/appInfoFailTest8.info"

appInfoFailExtension9 :: String
appInfoFailExtension9 = "/appInfoFailTest9.info"
