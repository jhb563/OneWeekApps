-- This module tests the parseXFromFile methods of our parsers under particular
-- test cases. These cases involve a larger amount of space between attribute
-- names and values (which should succeed), and error cases with improper
-- indentation, such as no indentation or changing indentation.

module SpacingIndentTests (
  runSpacingIndentTests
) where

import OWAAlertParser
import OWAColorParser
import OWAErrorParser
import OWAFontParser
import TestSpacingIndentErrors
import TestUtil
import Test.Hspec

runSpacingIndentTests :: FilePath -> IO ()
runSpacingIndentTests currentDirectory = hspec $ do
  let testDirectory = currentDirectory ++ testDirectoryExtension
  spacedAttributeTests testDirectory
  emptyIndentTests testDirectory
  changingIndentTests testDirectory

spacedAttributeTests :: FilePath -> Spec
spacedAttributeTests testDirectory = do
  let testFile1 = testDirectory ++ spacedColorsExtension
  let testFile2 = testDirectory ++ spacedFontsExtension
  let testFile3 = testDirectory ++ spacedAlertsExtension
  let testFile4 = testDirectory ++ spacedErrorsExtension
  describe "Parse objects which have any combination of spaces and tabs between attribute names and values" $ do
    context "when the objects are color objects" $
      it "Should successfully parse the file" $
        shouldReturnWithoutErrors (parseColorsFromFile testFile1)

    context "when the objects are font objects" $
      it "Should successfully parse the file" $
        shouldReturnWithoutErrors (parseFontsFromFile testFile2)

    context "when the objects are alert objects" $
      it "Should successfully parse the file" $
        shouldReturnWithoutErrors (parseAlertsFromFile testFile3)

    context "when the objects are error objects" $
      it "Should successfully parse the file" $
        shouldReturnWithoutErrors (parseErrorsFromFile testFile4)

emptyIndentTests :: FilePath -> Spec
emptyIndentTests testDirectory = do
  let testFile1 = testDirectory ++ emptyIndentColorsExtension
  let testFile2 = testDirectory ++ emptyIndentFontsExtension
  let testFile3 = testDirectory ++ emptyIndentAlertsExtension
  let testFile4 = testDirectory ++ emptyIndentErrorsExtension1
  let testFile5 = testDirectory ++ emptyIndentErrorsExtension2
  describe "Parse objects which fail to indent when listing their attributes" $ do
    context "when the objects are color objects" $
      it "Should return a parse error highlighting the indentation failure" $
        parseColorsFromFile testFile1 `shouldMatchError` emptyIndentColors

    context "when the objects are font objects" $
      it "Should return a parse error highlighting the indentation failure" $
        parseFontsFromFile testFile2 `shouldMatchError` emptyIndentFonts

    context "when the objects are alert objects" $
      it "Should return a parse error highlighting the indentation failure" $
        parseAlertsFromFile testFile3 `shouldMatchError` emptyIndentAlerts

    context "when the objects are default domain objects" $
      it "Should return a parse error highlighting the indentation failure" $
        parseErrorsFromFile testFile4 `shouldMatchError` emptyIndentErrors1

    context "when the objects are error objects" $
      it "Should return a parse error highlighting the indentation failure" $
        parseErrorsFromFile testFile5 `shouldMatchError` emptyIndentErrors2

changingIndentTests :: FilePath -> Spec
changingIndentTests testDirectory = do
  let testFile1 = testDirectory ++ changingIndentColorsExtension
  let testFile2 = testDirectory ++ changingIndentFontsExtension
  let testFile3 = testDirectory ++ changingIndentAlertsExtension
  let testFile4 = testDirectory ++ changingIndentErrorsExtension
  describe "Parse objects which change their indentation of attributes" $ do
    context "when the objects are color objects" $
      it "Should return a parse error highlighting the indentation failure" $
        parseColorsFromFile testFile1 `shouldMatchError` changingIndentColors

    context "when the objects are font objects" $
      it "Should return a parse error highlighting the indentation failure" $
        parseFontsFromFile testFile2 `shouldMatchError` changingIndentFonts

    context "when the objects are alert objects" $
      it "Should return a parse error highlighting the indentation failure" $
        parseAlertsFromFile testFile3 `shouldMatchError` changingIndentAlerts

    context "when the objects are default domain objects" $
      it "Should return a parse error highlighting the indentation failure" $
        parseErrorsFromFile testFile4 `shouldMatchError` changingIndentErrors
  
testDirectoryExtension :: String
testDirectoryExtension = "/tests/Version015Tests/SpacingIndentTests/ParseFiles"

spacedColorsExtension :: String
spacedColorsExtension = "/spacedColors.colors"

spacedFontsExtension :: String
spacedFontsExtension = "/spacedFonts.fonts"

spacedAlertsExtension :: String
spacedAlertsExtension = "/spacedAlerts.alerts"

spacedErrorsExtension :: String
spacedErrorsExtension = "/spacedErrors.errors"

emptyIndentColorsExtension :: String
emptyIndentColorsExtension = "/emptyIndentColors.colors"

emptyIndentFontsExtension :: String
emptyIndentFontsExtension = "/emptyIndentFonts.fonts"

emptyIndentAlertsExtension :: String
emptyIndentAlertsExtension = "/emptyIndentAlerts.alerts"

emptyIndentErrorsExtension1 :: String
emptyIndentErrorsExtension1 = "/emptyIndentErrors1.errors"

emptyIndentErrorsExtension2 :: String
emptyIndentErrorsExtension2 = "/emptyIndentErrors2.errors"

changingIndentColorsExtension :: String
changingIndentColorsExtension = "/changingIndentColors.colors"

changingIndentFontsExtension :: String
changingIndentFontsExtension = "/changingIndentFonts.fonts"

changingIndentAlertsExtension :: String
changingIndentAlertsExtension = "/changingIndentAlerts.alerts"

changingIndentErrorsExtension :: String
changingIndentErrorsExtension = "/changingIndentErrors.errors"
