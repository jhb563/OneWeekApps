module ItemFailureTests (
  runItemFailureTests
) where

import Test.Hspec

import OWAAlertParser
import OWAColorParser
import OWAErrorParser
import OWAFontParser
import TestParseErrors
import TestUtil

runItemFailureTests :: FilePath -> IO ()
runItemFailureTests currentDirectory = hspec $ do
  let testDirectory = currentDirectory ++ "/tests/Version015Tests/ItemFailureTests/ItemFailureParseFiles"
  alertItemFailureTests testDirectory
  colorItemFailureTests testDirectory
  errorItemFailureTests testDirectory
  fontItemFailureTests testDirectory

alertItemFailureTests :: FilePath -> Spec
alertItemFailureTests testDirectory = do
  let alertTestsFile = testDirectory ++ alertsExtension
  describe "Item Failures on Alerts" $
    it "Should match our list of alert failures" $
      parseAlertsFromFile alertTestsFile `shouldReturnLefts` allAlertErrors

colorItemFailureTests :: FilePath -> Spec
colorItemFailureTests testDirectory = do
  let colorTestsFile = testDirectory ++ colorsExtension
  describe "Item Failures on Colors" $
    it "Should match our list of color failures" $
      parseColorsFromFile colorTestsFile `shouldReturnLefts` allColorErrors

errorItemFailureTests :: FilePath -> Spec
errorItemFailureTests testDirectory = do
  let errorTestsFile = testDirectory ++ errorsExtension
  describe "Item Failures on Errors" $
    it "Should match our list of error failures" $
      parseErrorsFromFile errorTestsFile `shouldReturnLefts` allErrorErrors 

fontItemFailureTests :: FilePath -> Spec
fontItemFailureTests testDirectory = do
  let fontTestsFile = testDirectory ++ fontsExtension
  describe "Item Failures on Fonts" $
    it "Should match our list of font failures" $
      parseFontsFromFile fontTestsFile `shouldReturnLefts` allFontErrors

alertsExtension :: String
alertsExtension = "/alertFailures.alerts"

colorsExtension :: String
colorsExtension = "/colorFailures.colors"

errorsExtension :: String
errorsExtension = "/errorFailures.errors"

fontsExtension :: String
fontsExtension = "/fontFailures.fonts"
