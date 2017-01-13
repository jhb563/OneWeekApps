-- This will test parseAlertsFromFile, out of the module
-- OWAAlertParser, under conditions which should create
-- a parse failure.

module AlertParseFailureTests (
  runAlertParseFailureTests
) where

import Test.Hspec

import Parse.OWAAlertParser
import TestAlertErrors
import TestUtil

runAlertParseFailureTests :: FilePath -> IO ()
runAlertParseFailureTests currentDirectory = hspec $ do
  let testDirectory = currentDirectory ++ testDirectoryExtension 
  alertKeywordTest testDirectory
  alertNameTest testDirectory
  badAttributeTest testDirectory
  badLocalizedKeyTest testDirectory
  badButtonKeyTest testDirectory
  newLineTest testDirectory

alertKeywordTest :: FilePath -> Spec
alertKeywordTest testDirectory = do
  let testFile1 = testDirectory ++ alertKeyword1Extension
  let testFile2 = testDirectory ++ alertKeyword2Extension
  describe "Parse alert file which has improper keyword in place of \"Alert\"" $ do
    context "when that keyword is at the start of the file" $
      it "Should return a parse error highlighting the improper keyword" $
        parseAlertsFromFile testFile1 `shouldMatchError` alertKeywordFailure1

    context "when the keyword follow a correctly formatted alert" $
      it "Should return a parse error highlighting the improper keyword" $
        parseAlertsFromFile testFile2 `shouldMatchError` alertKeywordFailure2

alertNameTest :: FilePath -> Spec
alertNameTest testDirectory = do
  let testFile1 = testDirectory ++ alertName1Extension
  let testFile2 = testDirectory ++ alertName2Extension
  describe "Parse alert file which has improperly named Alert" $ do
    context "when that name starts with a number" $
      it "Should return a parse error highlighting the improper name" $
        parseAlertsFromFile testFile1 `shouldMatchError` alertNameFailure1

    context "when that name begins with a uppercase letter" $
      it "Should return a parse error highlighting the improper name" $
        parseAlertsFromFile testFile2 `shouldMatchError` alertNameFailure2  

badAttributeTest :: FilePath -> Spec
badAttributeTest testDirectory = do
  let testFile1 = testDirectory ++ badAttribute1Extension
  let testFile2 = testDirectory ++ badAttribute2Extension
  describe "Parse alert file which has improperly named attribute" $ do
    context "when that attribute is a made up word" $
      it "Should return a parse error highlighting the improper attribute name" $
        parseAlertsFromFile testFile1 `shouldMatchError` badAttributeFailure1

    context "when that attribute matches a correct attribute but has incorrect capitalization" $
      it "Should return a parse error highlighting the improper attribute name" $
        parseAlertsFromFile testFile2 `shouldMatchError` badAttributeFailure2

badLocalizedKeyTest :: FilePath -> Spec
badLocalizedKeyTest testDirectory = do
  let testFile1 = testDirectory ++ badLocalizedKey1Extension
  let testFile2 = testDirectory ++ badLocalizedKey2Extension
  describe "Parse alert file which has improperly formatted localized key" $ do
    context "when that key is for title, and not surrounded by quotes" $
      it "Should return a parse error highlighting the improper key" $
        parseAlertsFromFile testFile1 `shouldMatchError` badLocalizedKeyFailure1

    context "when that key is for message, and does not escape inner quotes" $
      it "Should return a parse error highlighting the improper key" $
        parseAlertsFromFile testFile2 `shouldMatchError` badLocalizedKeyFailure2

badButtonKeyTest :: FilePath -> Spec
badButtonKeyTest testDirectory = do
  let testFile1 = testDirectory ++ badButtonKey1Extension
  let testFile2 = testDirectory ++ badButtonKey2Extension
  let testFile3 = testDirectory ++ badButtonKey3Extension
  let testFile4 = testDirectory ++ badButtonKey4Extension
  describe "Parse alert file which has bad key values for buttons" $ do
    context "when the key is for a NeutralButton" $
      it "Should return a parse error highlighting the improper key" $
        parseAlertsFromFile testFile1 `shouldMatchError` badButtonKeyFailure1

    context "when the key is for a DismissButton" $
      it "Should return a parse error highlighting the improper key" $
        parseAlertsFromFile testFile2 `shouldMatchError` badButtonKeyFailure2

    context "when the key is for a YesButton" $
      it "Should return a parse error highlighting the improper key" $
        parseAlertsFromFile testFile3 `shouldMatchError` badButtonKeyFailure3

    context "when the key is for a NoButton" $
      it "Should return a parse error highlighting the improper key" $
        parseAlertsFromFile testFile4 `shouldMatchError` badButtonKeyFailure4
  
newLineTest :: FilePath -> Spec
newLineTest testDirectory = do
  let testFile1 = testDirectory ++ newLineEndExtension
  describe "Parse alert file which does not end in a new line character" $
    it "Should return a parse error highlighting the lack of a new line character" $
      parseAlertsFromFile testFile1 `shouldMatchError` newLineEndFailure

testDirectoryExtension :: FilePath
testDirectoryExtension = "/tests/Version015Tests/AlertParseFailureTests/ParseFiles"

alertKeyword1Extension :: FilePath
alertKeyword1Extension = "/alertKeywordFailure1.alerts"

alertKeyword2Extension :: FilePath
alertKeyword2Extension = "/alertKeywordFailure2.alerts"

alertName1Extension :: FilePath
alertName1Extension = "/alertNameFailure1.alerts"

alertName2Extension :: FilePath
alertName2Extension = "/alertNameFailure2.alerts"

badAttribute1Extension :: FilePath
badAttribute1Extension = "/badAttributeFailure1.alerts"

badAttribute2Extension :: FilePath
badAttribute2Extension = "/badAttributeFailure2.alerts"

badLocalizedKey1Extension :: FilePath
badLocalizedKey1Extension = "/badLocalizedKeyFailure1.alerts"

badLocalizedKey2Extension :: FilePath
badLocalizedKey2Extension = "/badLocalizedKeyFailure2.alerts"

badButtonKey1Extension :: FilePath
badButtonKey1Extension = "/badButtonKeyFailure1.alerts"

badButtonKey2Extension :: FilePath
badButtonKey2Extension = "/badButtonKeyFailure2.alerts"

badButtonKey3Extension :: FilePath
badButtonKey3Extension = "/badButtonKeyFailure3.alerts"

badButtonKey4Extension :: FilePath
badButtonKey4Extension = "/badButtonKeyFailure4.alerts"

newLineEndExtension :: FilePath
newLineEndExtension = "/newLineEndFailure.alerts"
