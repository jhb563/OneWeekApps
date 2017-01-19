-- This module will test the function:
-- parseStringsFromFile :: FilePath -> IO Either [OWAParseError] (Map String String)
-- from StringsParser, which takes a file and returns a
-- map of localized strings in that file.

module Parse.Tests.Strings.Basic (
  runStringsParseTests
) where

import Test.Hspec

import Parse.StringsParser
import Parse.Tests.Strings.Errors
import Parse.Tests.Strings.Objects
import Parse.Tests.Utils (shouldMatchError, shouldReturnRights)

runStringsParseTests :: FilePath -> IO ()
runStringsParseTests currentDirectory = hspec $ do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  passingStringsTests parseDirectory
  failingStringsTests parseDirectory

passingStringsTests :: FilePath -> Spec
passingStringsTests parseDirectory = do
  let testFile1 = parseDirectory ++ passExtension1
  let testFile2 = parseDirectory ++ passExtension2
  let testFile3 = parseDirectory ++ passExtension3
  let testFile4 = parseDirectory ++ passExtension4
  describe "Parse localized strings from correctly formatted files" $ do
    context "When the strings are in a single block, and may or may not have semicolons" $
      it "Should return the map of strings in the file" $
        parseStringsFromFile testFile1 `shouldReturnRights` basicStrings

    context "When the keys and values of the strings contain escaped quotes" $
      it "Should return the map of strings in the file" $
        parseStringsFromFile testFile2 `shouldReturnRights` quotedStrings

    context "When the blocks of the file and strings definitions are spaced out" $
      it "Should return the map of strings in the file" $
        parseStringsFromFile testFile3 `shouldReturnRights` spacedStrings

    context "When there are comment through the file" $
      it "Should return the map of strings in the file" $
        parseStringsFromFile testFile4 `shouldReturnRights` commentedStrings

failingStringsTests :: FilePath -> Spec
failingStringsTests parseDirectory = do
  let testFile1 = parseDirectory ++ failExtension1
  let testFile2 = parseDirectory ++ failExtension2
  let testFile3 = parseDirectory ++ failExtension3
  describe "Parse strings files when the files contain parse errors" $ do
    context "When a key is unquoted" $
      it "Should return an error highlighting the invalid key" $
        parseStringsFromFile testFile1 `shouldMatchError` keyFailError

    context "When a value is unquoted" $
      it "Should return an error highlighting the invalid word" $
        parseStringsFromFile testFile2 `shouldMatchError` wordFailError

    context "When quotes are unescaped" $
      it "Should return an error highlighting the offending quotes" $
        parseStringsFromFile testFile3 `shouldMatchError` quotesFailError

parseDirectoryExtension :: FilePath 
parseDirectoryExtension = "/test/Parse/Tests/Strings/ParseFiles"

passExtension1 :: String
passExtension1 = "/basicStringsTest.strings"

passExtension2 :: String
passExtension2 = "/quotedStringsTest.strings"

passExtension3 :: String
passExtension3 = "/spacedStringsTest.strings"

passExtension4 :: String
passExtension4 = "/commentedStringsTest.strings"

failExtension1 :: String
failExtension1 = "/keyFailTest.strings"

failExtension2 :: String
failExtension2 = "/wordFailTest.strings"

failExtension3 :: String
failExtension3 = "/quotesFailTest.strings"
