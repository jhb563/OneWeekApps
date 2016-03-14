-- We test the same parse methods as version 0.1.0, only now
-- the files being parsed use variable ways of parsing.

module TabTests (
  runTabTests
) where

import OWAAlertParser
import OWAColorParser
import OWAErrorParser
import OWAFontParser
import TestUtil
import Test.Hspec

runTabTests :: FilePath -> IO ()
runTabTests currentDirectory = hspec $ do
  let testDirectory = currentDirectory ++ "/tests/Version015Tests/TabTests/TabParseFiles"
  spaceTabTests testDirectory
  tabTabTests testDirectory
  cominbationTabTests testDirectory
  multipleFormatTabTests testDirectory

spaceTabTests :: FilePath -> Spec
spaceTabTests testDirectory = do
  let spaceTabTestFile = testDirectory ++ spaceTabTestFileExtension
  describe "Parse alerts with varying lengths of indentation" $
    context "when the indentation is entirely spaces" $
      it "Should successfully parse, returning a non-emtpy list" $
        shouldReturnWithoutErrors $ parseAlertsFromFile spaceTabTestFile

tabTabTests :: FilePath -> Spec
tabTabTests testDirectory = do
  let tabTabTestFile = testDirectory ++ tabTabTestFileExtension
  describe "Parse colors with varying lengths of indentation" $
    context "when the indentation is entirely tabs" $
      it "Should successfully parse, returning a non-empty list" $
        shouldReturnWithoutErrors $ parseColorsFromFile tabTabTestFile

cominbationTabTests :: FilePath -> Spec
cominbationTabTests testDirectory = do
  let cominbationTabTestFile = testDirectory ++ cominbationTabTestFileExtension
  describe "Parse errors with varying lengths of indentation" $
    context "when the indentation is a combination of spaces and tabs" $
      it "Should successfully parse, returning a non-empty list" $
        shouldReturnNonEmpty $ parseErrorsFromFile cominbationTabTestFile

multipleFormatTabTests :: FilePath -> Spec
multipleFormatTabTests testDirectory = do
  let multipleFormatTabTestsFile = testDirectory ++ multipleFormatTabTestsFileExtension
  describe "Parse fonts with varying lengths and styles of indentation" $
    it "Should successfully parse, returning a non-empty list" $
      shouldReturnWithoutErrors $ parseFontsFromFile multipleFormatTabTestsFile

spaceTabTestFileExtension :: String
spaceTabTestFileExtension = "/spaceTabTest.alerts"

tabTabTestFileExtension :: String
tabTabTestFileExtension = "/tabTabTest.colors"

cominbationTabTestFileExtension :: String
cominbationTabTestFileExtension = "/combinationTabTest.errors"

multipleFormatTabTestsFileExtension :: String
multipleFormatTabTestsFileExtension = "/multipleTabTest.fonts"
