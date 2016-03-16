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

colorKeywordTest :: FilePath -> Spec
colorKeywordTest testDirectory = do
  let testFile1 = testDirectory ++ colorKeyword1Extension
  let testFile2 = testDirectory ++ colorKeyword2Extension
  describe "Parse color file which has improper keyword in place of \"Color\"" $ do
    context "when that keyword is at the start of the file" $
      it "Should return a parse error highlighting the improper keyword" $
        parseColorsFromFile testFile1 `shouldMatchErrorInfo` (errorFromInfo testDirectory colorKeyword1FailureInfo)

    context "when the keyword follow a correctly formatted color" $
      it "Should return a parse error highlighting the improper keyword" $
        parseColorsFromFile testFile2 `shouldMatchErrorInfo` (errorFromInfo testDirectory colorKeyword2FailureInfo)

testDirectoryExtension :: FilePath
testDirectoryExtension = "/tests/Version015Tests/ColorParseFailureTests/ParseFiles"

colorKeyword1Extension :: FilePath
colorKeyword1Extension = "/colorKeywordFailure1.colors"

colorKeyword2Extension :: FilePath
colorKeyword2Extension = "/colorKeywordFailure2.colors"
