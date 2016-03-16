-- This will test parseColorsFromFile, out of the module
-- OWAColorParser, under conditions which should create
-- a parse failure.

module ColorParseFailureTests (
  runColorParseFailureTests
) where

import OWAColorParser
import OWAParseError
import TestColorErrors
import TestUtil
import Test.Hspec

runColorParseFailureTests :: FilePath -> IO ()
runColorParseFailureTests currentDirectory = hspec $ do
  let testDirectory = currentDirectory ++ testDirectoryExtension 
  colorKeywordTest testDirectory
  colorNameTest testDirectory
  badAttributeTest testDirectory
  floatAttributeTest testDirectory
  hexAttributeTest testDirectory
  newLineTest testDirectory

colorKeywordTest :: FilePath -> Spec
colorKeywordTest testDirectory = do
  let testFile1 = testDirectory ++ colorKeyword1Extension
  let testFile2 = testDirectory ++ colorKeyword2Extension
  describe "Parse color file which has improper keyword in place of \"Color\"" $ do
    context "when that keyword is at the start of the file" $
      it "Should return a parse error highlighting the improper keyword" $
        parseColorsFromFile testFile1 `shouldMatchError` (errorFromInfo colorKeyword1FailureInfo)

    context "when the keyword follow a correctly formatted color" $
      it "Should return a parse error highlighting the improper keyword" $
        parseColorsFromFile testFile2 `shouldMatchError` (errorFromInfo colorKeyword2FailureInfo)

colorNameTest :: FilePath -> Spec
colorNameTest testDirectory = do
  let testFile1 = testDirectory ++ colorName1Extension
  let testFile2 = testDirectory ++ colorName2Extension
  let testFile3 = testDirectory ++ colorName3Extension
  describe "Parse color file which has improperly named Color" $ do
    context "when that name begins with a number" $
      it "Should return a parse error highlighting the improper name" $
        parseColorsFromFile testFile1 `shouldMatchError` (errorFromInfo colorName1FailureInfo)  

    context "when that name begins with a uppercase letter" $
      it "Should return a parse error highlighting the improper name" $
        parseColorsFromFile testFile2 `shouldMatchError` (errorFromInfo colorName2FailureInfo)  

    context "when that name begins with a special character" $
      it "Should return a parse error highlighting the improper name" $
        parseColorsFromFile testFile3 `shouldMatchError` (errorFromInfo colorName3FailureInfo)  

badAttributeTest :: FilePath -> Spec
badAttributeTest testDirectory = do
  let testFile1 = testDirectory ++ badAttribute1Extension
  let testFile2 = testDirectory ++ badAttribute2Extension
  describe "Parse color file which has improperly named attribute" $ do
    context "when that attribute is a made up word" $
      it "Should return a parse error highlighting the improper attribute name" $
        parseColorsFromFile testFile1 `shouldMatchError` (errorFromInfo badAttribute1FailureInfo)

    context "when that attribute matches a correct attribute but has incorrect capitalization" $
      it "Should return a parse error highlighting the improper attribute name" $
        parseColorsFromFile testFile2 `shouldMatchError` (errorFromInfo badAttribute2FailureInfo)

floatAttributeTest :: FilePath -> Spec
floatAttributeTest testDirectory = do
  let testFile1 = testDirectory ++ float1Extension
  let testFile2 = testDirectory ++ float2Extension
  let testFile3 = testDirectory ++ float3Extension
  describe "Parse color file which has invalid float attribute" $ do
    context "when that attribute uses a hanging decimal" $
      it "Should return a parse error highlighting the improper float value" $
        parseColorsFromFile testFile1 `shouldMatchError` (errorFromInfo float1FailureInfo)

    context "when that attribute uses a negative number" $
      it "Should return a parse error highlighting the improper float value" $
        parseColorsFromFile testFile2 `shouldMatchError` (errorFromInfo float2FailureInfo)

    context "when that attribute uses scientific notation" $
      it "Should return a parse error highlighting the improper float value" $
        parseColorsFromFile testFile3 `shouldMatchError` (errorFromInfo float3FailureInfo)

hexAttributeTest :: FilePath -> Spec
hexAttributeTest testDirectory = do
  let testFile1 = testDirectory ++ hex1Extension
  let testFile2 = testDirectory ++ hex2Extension
  let testFile3 = testDirectory ++ hex3Extension
  let testFile4 = testDirectory ++ hex4Extension
  describe "Parse color file which has invalid hex attribute" $ do
    context "when that attribute does not start with \"0x\"" $
      it "Should return a parse error highlighting the improper hex value" $
        parseColorsFromFile testFile1 `shouldMatchError` (errorFromInfo hex1FailureInfo)

    context "when the hex attribute has only too few characters" $
      it "Should return a parse error highlighting the improper hex value" $
        parseColorsFromFile testFile2 `shouldMatchError` (errorFromInfo hex2FailureInfo)

    context "when the hex attribute has an in between number of characters" $
      it "Should return a parse error highlighting the improper hex value" $
        parseColorsFromFile testFile3 `shouldMatchError` (errorFromInfo hex3FailureInfo)

    context "when the hex attribute has too many characters" $
      it "Should return a parse error highlighting the improper hex value" $
        parseColorsFromFile testFile4 `shouldMatchError` (errorFromInfo hex4FailureInfo)

newLineTest :: FilePath -> Spec
newLineTest testDirectory = do
  let testFile1 = testDirectory ++ newLineEndExtension
  describe "Parse color file which does not end in a new line character" $ do
    it "Should return a parse error highlighting the lack of a new line character" $
      parseColorsFromFile testFile1 `shouldMatchError` (errorFromInfo newLineFailureInfo)

testDirectoryExtension :: FilePath
testDirectoryExtension = "/tests/Version015Tests/ColorParseFailureTests/ParseFiles"

colorKeyword1Extension :: FilePath
colorKeyword1Extension = "/colorKeyword1Failure.colors"

colorKeyword2Extension :: FilePath
colorKeyword2Extension = "/colorKeyword2Failure.colors"

colorName1Extension :: FilePath
colorName1Extension = "/colorName1Failure.colors"

colorName2Extension :: FilePath
colorName2Extension = "/colorName2Failure.colors"

colorName3Extension :: FilePath
colorName3Extension = "/colorName3Failure.colors"

badAttribute1Extension :: FilePath
badAttribute1Extension = "/badAttribute1Failure.colors"

badAttribute2Extension :: FilePath
badAttribute2Extension = "/badAttribute2Failure.colors"

float1Extension :: FilePath
float1Extension = "/float1Failure.colors"

float2Extension :: FilePath
float2Extension = "/float2Failure.colors"

float3Extension :: FilePath
float3Extension = "/float3Failure.colors"

hex1Extension :: FilePath
hex1Extension = "/hex1Failure.colors"

hex2Extension :: FilePath
hex2Extension = "/hex2Failure.colors"

hex3Extension :: FilePath
hex3Extension = "/hex3Failure.colors"

hex4Extension :: FilePath
hex4Extension = "/hex4Failure.colors"

newLineEndExtension :: FilePath
newLineEndExtension = "/newLineEndFailure.colors"
