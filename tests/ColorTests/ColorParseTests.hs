-- OWAColorParser will expose the method:
-- parseColorsFromFile :: FilePath -> IO [OWAColor]
-- which will read a file and return a list of color
-- objects for the colors described in the file

module ColorParseTests (
  runColorParseTests
) where

import OWAColor
import OWAColorParser
import TestColors
import Test.Hspec

runColorParseTests :: FilePath -> IO ()
runColorParseTests startFilePath = hspec $ do
  let testDirectory = startFilePath ++ "/tests/ColorTests/ColorParseTests"
  rgbTests testDirectory
  rgbaTests testDirectory
  rgbHexTests testDirectory
  rgbaHexTests testDirectory
  hexAlphaTests testDirectory
  mixFormatInFileTests testDirectory

rgbTests :: FilePath -> Spec
rgbTests testDirectory = do
  let rgbTestsFile = testDirectory ++ rgbTestsExtension
  describe "Parse Colors from RGB Format" $
    context "when colors might be in any order" $
      context "with any number of new lines between colors" $
        it "Should match our list of rgbColors" $
          parseColorsFromFile rgbTestsFile `shouldReturn` rgbColorsList

rgbaTests :: FilePath -> Spec
rgbaTests testDirectory = do
  let rgbaTestsFile = testDirectory ++ rgbaTestsExtension
  describe "Parse Colors from RGBA Format" $
    context "when attributes might be in any order" $
      it "Should match our list of rgbaColors" $
        parseColorsFromFile rgbaTestsFile `shouldReturn` rgbaColorsList

rgbHexTests :: FilePath -> Spec
rgbHexTests testDirectory = do
  let rgbHexTestsFile = testDirectory ++ rgbHexTestsExtension
  describe "Parse Colors from Hex RGB Format" $
    it "Should match our list of rgbHexColors" $
      parseColorsFromFile rgbHexTestsFile `shouldReturn` rgbHexColorsList

rgbaHexTests :: FilePath -> Spec
rgbaHexTests testDirectory = do
  let rgbaHexTestsFile = testDirectory ++ rgbaHexTestsExtension
  describe "Parse Colors from Hex RGBA Format" $
    it "Should match our list of rgbaHexColors" $
      parseColorsFromFile rgbaHexTestsFile `shouldReturn` rgbaHexColorsList

hexAlphaTests :: FilePath -> Spec
hexAlphaTests testDirectory = do
  let hexAlphaTestsFile = testDirectory ++ hexAlphaTestsExtension
  describe "Parse Colors from Hex + Alpha Format" $
    it "Should match our list of hexAlphaColors" $
      parseColorsFromFile hexAlphaTestsFile `shouldReturn` hexAlphaColorsList

mixFormatInFileTests :: FilePath -> Spec
mixFormatInFileTests testDirectory = do
  let mixFormatFile = testDirectory ++ mixFormatExtension
  describe "Parse Colors from different formats in same file" $
    it "Should match our list of mixed format colors" $
      parseColorsFromFile mixFormatFile `shouldReturn` mixFormatColorsList

rgbTestsExtension :: String
rgbTestsExtension = "/rgbTests.colors"

rgbaTestsExtension :: String
rgbaTestsExtension = "/rgbaTests.colors"

rgbHexTestsExtension :: String
rgbHexTestsExtension = "/rgbHexTests.colors"

rgbaHexTestsExtension :: String
rgbaHexTestsExtension = "/rgbaHexTests.colors"

hexAlphaTestsExtension :: String
hexAlphaTestsExtension = "/hexAlphaTests.colors"

mixFormatExtension :: String
mixFormatExtension = "/mixFormatTests.colors"

-- TODO (0.1.2):
-- Failure Cases
-- Keywords:
-- Color, Red, Green, Blue, Alpha, Hex
-- Naming a color by a Keyword or other invalid name (duplicates)
-- Putting words instead numbers (line numbers)
-- Invalid combination of keywords
-- Failure to tab
-- Out of bounds (negative and postive) (give a warning)
