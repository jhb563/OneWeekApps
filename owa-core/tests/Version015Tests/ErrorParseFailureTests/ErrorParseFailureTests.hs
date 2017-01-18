-- This will test parseErrorsFromFile, out of the module
-- ErrorParser, under conditions which should create
-- a parse failure.

module ErrorParseFailureTests (
  runErrorParseFailureTests
) where

import Test.Hspec

import Parse.ErrorParser
import TestErrorErrors
import TestUtil

runErrorParseFailureTests :: FilePath -> IO ()
runErrorParseFailureTests currentDirectory = hspec $ do
  let testDirectory = currentDirectory ++ testDirectoryExtension 
  errorKeywordTest testDirectory
  errorNameTest testDirectory
  badAttributeTest testDirectory
  badCodeValueTest testDirectory
  badDescriptionValueTest testDirectory
  badDomainValueTest testDirectory
  badPrefixValueTest testDirectory
  newLineTest testDirectory

errorKeywordTest :: FilePath -> Spec
errorKeywordTest testDirectory = do
  let testFile1 = testDirectory ++ errorKeyword1Extension
  let testFile2 = testDirectory ++ errorKeyword2Extension
  let testFile3 = testDirectory ++ errorKeyword3Extension
  describe "Parse error file which has improper keyword in place of \"Error\"" $ do
    context "when that keyword is at the start of the file" $
      it "Should return a parse error highlighting the improper keyword" $
        parseErrorsFromFile testFile1 `shouldMatchError` errorKeywordFailure1

    context "when the keyword follow a correctly formatted error" $
      it "Should return a parse error highlighting the improper keyword" $
        parseErrorsFromFile testFile2 `shouldMatchError` errorKeywordFailure2

    context "when the keyword follow a correctly formatted default domain" $
      it "Should return a parse error highlighting the improper keyword" $
        parseErrorsFromFile testFile3 `shouldMatchError` errorKeywordFailure3

errorNameTest :: FilePath -> Spec
errorNameTest testDirectory = do
  let testFile1 = testDirectory ++ errorName1Extension
  let testFile2 = testDirectory ++ errorName2Extension
  describe "Parse error file which has improperly named object" $ do
    context "when an error name contains special characters" $
      it "Should return a parse error highlighting the improper name" $
        parseErrorsFromFile testFile1 `shouldMatchError` errorNameFailure1

    context "when a default domain name contains special characters" $
      it "Should return a parse error highlighting the improper name" $
        parseErrorsFromFile testFile2 `shouldMatchError` errorNameFailure2  

badAttributeTest :: FilePath -> Spec
badAttributeTest testDirectory = do
  let testFile1 = testDirectory ++ badAttribute1Extension
  let testFile2 = testDirectory ++ badAttribute2Extension
  let testFile3 = testDirectory ++ badAttribute3Extension
  let testFile4 = testDirectory ++ badAttribute4Extension
  describe "Parse error file which has improperly named attribute" $ do
    context "when that attribute is a made up word for a default domain object" $
      it "Should return a parse error highlighting the improper attribute name" $
        parseErrorsFromFile testFile1 `shouldMatchError` badAttributeFailure1

    context "when a default domain attribute matches a correct attribute but has incorrect capitalization" $
      it "Should return a parse error highlighting the improper attribute name" $
        parseErrorsFromFile testFile2 `shouldMatchError` badAttributeFailure2

    context "when that attribute is a made up word for a error object" $
      it "Should return a parse error highlighting the improper attribute name" $
        parseErrorsFromFile testFile3 `shouldMatchError` badAttributeFailure3

    context "when an error attribute matches a correct attribute but has incorrect capitalization" $
      it "Should return a parse error highlighting the improper attribute name" $
        parseErrorsFromFile testFile4 `shouldMatchError` badAttributeFailure4

badCodeValueTest :: FilePath -> Spec
badCodeValueTest testDirectory = do
  let testFile1 = testDirectory ++ badCodeValueExtension
  describe "Parse error file which has a bad code value" $
    it "Should return a parse error highlighting the bad value" $
      parseErrorsFromFile testFile1 `shouldMatchError` badCodeValueFailure

badDescriptionValueTest :: FilePath -> Spec
badDescriptionValueTest testDirectory = do
  let testFile1 = testDirectory ++ badDescriptionValueExtension
  describe "Parse error file which has a bad description value" $
    it "Should return a parse error highlighting the bad value" $
      parseErrorsFromFile testFile1 `shouldMatchError` badDescriptionValueFailure

badDomainValueTest :: FilePath -> Spec
badDomainValueTest testDirectory = do
  let testFile1 = testDirectory ++ badDomainValueExtension
  describe "Parse error file which has a bad domain value" $
    it "Should return a parse error highlighting the bad value" $
      parseErrorsFromFile testFile1 `shouldMatchError` badDomainValueFailure

badPrefixValueTest :: FilePath -> Spec
badPrefixValueTest testDirectory = do
  let testFile1 = testDirectory ++ badPrefixValueExtension
  describe "Parse error file which has a bad prefix value" $
    it "Should return a parse error highlighting the bad value" $
      parseErrorsFromFile testFile1 `shouldMatchError` badPrefixValueFailure

newLineTest :: FilePath -> Spec
newLineTest testDirectory = do
  let testFile1 = testDirectory ++ newLineEndExtension
  describe "Parse error file which does not end in a new line character" $
    it "Should return a parse error highlighting the lack of a new line character" $
      parseErrorsFromFile testFile1 `shouldMatchError` newLineEndFailure

testDirectoryExtension :: FilePath
testDirectoryExtension = "/tests/Version015Tests/ErrorParseFailureTests/ParseFiles"

errorKeyword1Extension :: FilePath
errorKeyword1Extension = "/errorKeywordFailure1.errors"

errorKeyword2Extension :: FilePath
errorKeyword2Extension = "/errorKeywordFailure2.errors"

errorKeyword3Extension :: FilePath
errorKeyword3Extension = "/errorKeywordFailure3.errors"

errorName1Extension :: FilePath
errorName1Extension = "/errorNameFailure1.errors"

errorName2Extension :: FilePath
errorName2Extension = "/errorNameFailure2.errors"

badAttribute1Extension :: FilePath
badAttribute1Extension = "/badAttributeFailure1.errors"

badAttribute2Extension :: FilePath
badAttribute2Extension = "/badAttributeFailure2.errors"

badAttribute3Extension :: FilePath
badAttribute3Extension = "/badAttributeFailure3.errors"

badAttribute4Extension :: FilePath
badAttribute4Extension = "/badAttributeFailure4.errors"

badCodeValueExtension :: FilePath
badCodeValueExtension = "/badCodeValueFailure.errors"

badDescriptionValueExtension :: FilePath
badDescriptionValueExtension = "/badDescriptionValueFailure.errors"

badDomainValueExtension :: FilePath
badDomainValueExtension = "/badDomainValueFailure.errors"

badPrefixValueExtension :: FilePath
badPrefixValueExtension = "/badPrefixValueFailure.errors"

newLineEndExtension :: FilePath
newLineEndExtension = "/newLineEndFailure.errors"
