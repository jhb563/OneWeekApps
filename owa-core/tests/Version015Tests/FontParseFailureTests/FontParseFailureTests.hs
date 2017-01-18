-- This will test parseFontsFromFile, out of the module
-- FontParser, under conditions which should create
-- a parse failure.

module FontParseFailureTests (
  runFontParseFailureTests
) where

import Test.Hspec

import Parse.FontParser
import TestFontErrors
import TestUtil

runFontParseFailureTests :: FilePath -> IO ()
runFontParseFailureTests currentDirectory = hspec $ do
  let testDirectory = currentDirectory ++ testDirectoryExtension 
  fontKeywordTest testDirectory
  fontNameTest testDirectory
  badAttributeTest testDirectory
  badFontFamilyTest testDirectory
  badFontSizeTest testDirectory
  badFontStylesTest testDirectory
  newLineTest testDirectory

fontKeywordTest :: FilePath -> Spec
fontKeywordTest testDirectory = do
  let testFile1 = testDirectory ++ fontKeyword1Extension
  let testFile2 = testDirectory ++ fontKeyword2Extension
  describe "Parse font file which has improper keyword in place of \"Font\"" $ do
    context "when that keyword is at the start of the file" $
      it "Should return a parse error highlighting the improper keyword" $
        parseFontsFromFile testFile1 `shouldMatchError` fontKeywordFailure1

    context "when the keyword follow a correctly formatted font" $
      it "Should return a parse error highlighting the improper keyword" $
        parseFontsFromFile testFile2 `shouldMatchError` fontKeywordFailure2

fontNameTest :: FilePath -> Spec
fontNameTest testDirectory = do
  let testFile1 = testDirectory ++ fontName1Extension
  let testFile2 = testDirectory ++ fontName2Extension
  describe "Parse font file which has improperly named Font" $ do
    context "when that name contains non alpha-numeric characters" $
      it "Should return a parse error highlighting the improper name" $
        parseFontsFromFile testFile1 `shouldMatchError` fontNameFailure1

    context "when that name begins with a uppercase letter" $
      it "Should return a parse error highlighting the improper name" $
        parseFontsFromFile testFile2 `shouldMatchError` fontNameFailure2  

badAttributeTest :: FilePath -> Spec
badAttributeTest testDirectory = do
  let testFile1 = testDirectory ++ badAttribute1Extension
  let testFile2 = testDirectory ++ badAttribute2Extension
  describe "Parse font file which has improperly named attribute" $ do
    context "when that attribute is a made up word" $
      it "Should return a parse error highlighting the improper attribute name" $
        parseFontsFromFile testFile1 `shouldMatchError` badAttributeFailure1

    context "when that attribute matches a correct attribute but has incorrect capitalization" $
      it "Should return a parse error highlighting the improper attribute name" $
        parseFontsFromFile testFile2 `shouldMatchError` badAttributeFailure2

badFontFamilyTest :: FilePath -> Spec
badFontFamilyTest testDirectory = do
  let testFile1 = testDirectory ++ badFontFamilyExtension
  describe "Parse font file which has special characters in font family name" $
    it "Should return a parse error highlighting the improper name" $
      parseFontsFromFile testFile1 `shouldMatchError` badFontFamilyFailure

badFontSizeTest :: FilePath -> Spec
badFontSizeTest testDirectory = do
  let testFile1 = testDirectory ++ badFontSize1Extension
  let testFile2 = testDirectory ++ badFontSize2Extension
  describe "Parse font file which has badly formatted bad font sizes" $ do
    context "when the given size uses scientific notation" $
      it "Should return a parse error highlighting the improper attribute" $
        parseFontsFromFile testFile1 `shouldMatchError` badFontSizeFailure1

    context "when the give size is a negative number" $
      it "Should return a parse error highlighting the improper attribute" $
        parseFontsFromFile testFile2 `shouldMatchError` badFontSizeFailure2

badFontStylesTest :: FilePath -> Spec
badFontStylesTest testDirectory = do
  let testFile1 = testDirectory ++ badFontStyles1Extension
  let testFile2 = testDirectory ++ badFontStyles2Extension
  describe "Parse font file which has badly formatted style attributes" $ do
    context "when the given styles have no commas between them" $
      it "Should return a parse error highlighting the improper attribute" $
        parseFontsFromFile testFile1 `shouldMatchError` badFontStylesFailure1

    context "when the given styles use unknown words" $
      it "Should return a parse error highlighting the improper attribute" $
        parseFontsFromFile testFile2 `shouldMatchError` badFontStylesFailure2

newLineTest :: FilePath -> Spec
newLineTest testDirectory = do
  let testFile1 = testDirectory ++ newLineEndExtension
  describe "Parse font file which does not end in a new line character" $
    it "Should return a parse error highlighting the lack of a new line character" $
      parseFontsFromFile testFile1 `shouldMatchError` newLineEndFailure

testDirectoryExtension :: FilePath
testDirectoryExtension = "/tests/Version015Tests/FontParseFailureTests/ParseFiles"

fontKeyword1Extension :: FilePath
fontKeyword1Extension = "/fontKeywordFailure1.fonts"

fontKeyword2Extension :: FilePath
fontKeyword2Extension = "/fontKeywordFailure2.fonts"

fontName1Extension :: FilePath
fontName1Extension = "/fontNameFailure1.fonts"

fontName2Extension :: FilePath
fontName2Extension = "/fontNameFailure2.fonts"

badAttribute1Extension :: FilePath
badAttribute1Extension = "/badAttributeFailure1.fonts"

badAttribute2Extension :: FilePath
badAttribute2Extension = "/badAttributeFailure2.fonts"

badFontFamilyExtension :: FilePath
badFontFamilyExtension = "/badFontFamilyFailure.fonts"

badFontSize1Extension :: FilePath
badFontSize1Extension = "/badFontSizeFailure1.fonts"

badFontSize2Extension :: FilePath
badFontSize2Extension = "/badFontSizeFailure2.fonts"

badFontStyles1Extension :: FilePath
badFontStyles1Extension = "/badFontStylesFailure1.fonts"

badFontStyles2Extension :: FilePath
badFontStyles2Extension = "/badFontStylesFailure2.fonts"

newLineEndExtension :: FilePath
newLineEndExtension = "/newLineEndFailure.fonts"
