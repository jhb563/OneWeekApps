-- OWAFontParse will expose the method:
-- parseFontsFromFile :: FilePath -> IO [OWAFont]
-- which will read a file and return a list of font
-- objects for the fonts described in the file

module FontParseTests (
  runFontParseTests
) where

import OWAFont
import OWAFontParser
import TestFonts
import Test.Hspec

runFontParseTests :: FilePath -> IO ()
runFontParseTests startFilePath = hspec $ do
  let parseFilesPath = startFilePath ++ "/tests/FontTests/FontParseFiles" 
  regularFontTests parseFilesPath
  spacedFontTests parseFilesPath
  multiStyleFontTests parseFilesPath
  noStyleFontTests parseFilesPath

regularFontTests :: FilePath -> Spec
regularFontTests testDirectory = do
  let regularFontTestsFile = testDirectory ++ regularFontsExtension
  describe "Parse Fonts from Regular Fonts File" $
    it "Should match our list of regular fonts" $
      parseFontsFromFile regularFontTestsFile `shouldReturn` regularFontsList

spacedFontTests :: FilePath -> Spec
spacedFontTests testDirectory = do
  let spacedFontTestsFile = testDirectory ++ spacedFontsExtension
  describe "Parse Fonts in regular format" $
    context "with any amount of spacing between them" $
      it "Should match our list of spaced fonts" $
        parseFontsFromFile spacedFontTestsFile `shouldReturn` spacedFontsList

multiStyleFontTests :: FilePath -> Spec
multiStyleFontTests testDirectory = do
  let multiStyleFontTestsFile = testDirectory ++ multiStyleFontsExtension
  describe "Parse Fonts with multiple styles" $
    it "Should match our list of multi-style fonts" $
      parseFontsFromFile multiStyleFontTestsFile `shouldReturn` multiStyleFontsList

noStyleFontTests :: FilePath -> Spec
noStyleFontTests testDirectory = do
  let noStyleFontTestsFile = testDirectory ++ noStyleFontsExtension
  describe "Parse Fonts with no style attributes" $
    context "when attributes might be included in the family name" $
      it "Should match our list of no-style fonts" $
        parseFontsFromFile noStyleFontTestsFile `shouldReturn` noStyleFontsList

regularFontsExtension :: String
regularFontsExtension = "/regularFonts.fonts"

spacedFontsExtension :: String
spacedFontsExtension = "/spacedFonts.fonts"

multiStyleFontsExtension :: String
multiStyleFontsExtension = "/multiStyleFonts.fonts"

noStyleFontsExtension :: String
noStyleFontsExtension = "/noStyleFonts.fonts"

regularFontsList :: [OWAFont]
regularFontsList = [myFont, titleFont, labelFont, thinFont]

spacedFontsList :: [OWAFont]
spacedFontsList = [placeholderFont, buttonFont]

multiStyleFontsList :: [OWAFont]
multiStyleFontsList = [textfieldFont, randomFont]

noStyleFontsList :: [OWAFont]
noStyleFontsList = [noStyleFont, styleInNameFont, timesFont]

