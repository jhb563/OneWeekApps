-- Parse.ColorParser will expose the method:
-- parseColorsFromFile :: FilePath -> IO [OWAColor]
-- which will read a file and return a list of color
-- objects for the colors described in the file

module Parse.Tests.Colors.Basic (
  runColorParseTests
) where

import Test.Hspec

import Parse.OWAColorParser (parseColorsFromFile)
import Parse.Tests.Colors.Objects
import Parse.Tests.Utils (shouldReturnRights)

runColorParseTests :: FilePath -> IO ()
runColorParseTests startFilePath = hspec $ do
  let testDirectory = startFilePath ++ "/test/Parse/Tests/Colors/ParseFiles"
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
          parseColorsFromFile rgbTestsFile `shouldReturnRights` rgbColorsList

rgbaTests :: FilePath -> Spec
rgbaTests testDirectory = do
  let rgbaTestsFile = testDirectory ++ rgbaTestsExtension
  describe "Parse Colors from RGBA Format" $
    context "when attributes might be in any order" $
      it "Should match our list of rgbaColors" $
        parseColorsFromFile rgbaTestsFile `shouldReturnRights` rgbaColorsList

rgbHexTests :: FilePath -> Spec
rgbHexTests testDirectory = do
  let rgbHexTestsFile = testDirectory ++ rgbHexTestsExtension
  describe "Parse Colors from Hex RGB Format" $
    it "Should match our list of rgbHexColors" $
      parseColorsFromFile rgbHexTestsFile `shouldReturnRights` rgbHexColorsList

rgbaHexTests :: FilePath -> Spec
rgbaHexTests testDirectory = do
  let rgbaHexTestsFile = testDirectory ++ rgbaHexTestsExtension
  describe "Parse Colors from Hex RGBA Format" $
    it "Should match our list of rgbaHexColors" $
      parseColorsFromFile rgbaHexTestsFile `shouldReturnRights` rgbaHexColorsList

hexAlphaTests :: FilePath -> Spec
hexAlphaTests testDirectory = do
  let hexAlphaTestsFile = testDirectory ++ hexAlphaTestsExtension
  describe "Parse Colors from Hex + Alpha Format" $
    it "Should match our list of hexAlphaColors" $
      parseColorsFromFile hexAlphaTestsFile `shouldReturnRights` hexAlphaColorsList

mixFormatInFileTests :: FilePath -> Spec
mixFormatInFileTests testDirectory = do
  let mixFormatFile = testDirectory ++ mixFormatExtension
  describe "Parse Colors from different formats in same file" $
    it "Should match our list of mixed format colors" $
      parseColorsFromFile mixFormatFile `shouldReturnRights` mixFormatColorsList

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
