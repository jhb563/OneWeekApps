-- OWAColorParser will expose the method:
-- parseColorsFromFile :: FilePath -> IO [OWAColor]
-- which will read a file and return a list of color
-- objects for the colors described in the file

module ColorParseTests (
  runColorParseTests
) where

import OWAColorParser
import OWAColor
import Test.Hspec

runColorParseTests :: FilePath -> IO ()
runColorParseTests startFilePath = hspec $ do
  let testDirectory = startFilePath ++ "/tests/ColorParseTests"
  _ <- rgbTests testDirectory
  _ <- rgbaTests testDirectory
  _ <- rgbHexTests testDirectory
  _ <- rgbaHexTests testDirectory
  _ <- hexAlphaTests testDirectory
  mixFormatInFileTests testDirectory

rgbTests :: FilePath -> Spec
rgbTests currentDirectory = do
  let rgbTestsFile = currentDirectory ++ rgbTestsExtension
  describe "Parse Colors from RGB Format" $ do
    context "when colors might be in any order" $
      context "with any number of new lines between colors" $
        it "Should match our list of rgbColors" $
          parseColorsFromFile rgbTestsFile `shouldReturn` rgbColorsList

rgbaTests :: FilePath -> Spec
rgbaTests currentDirectory = do
  let rgbaTestsFile = currentDirectory ++ rgbaTestsExtension
  describe "Parse Colors from RGBA Format" $ do
    context "when attributes might be in any order" $
      it "Should match our list of rgbaColors" $
        parseColorsFromFile rgbaTestsFile `shouldReturn` rgbaColorsList

rgbHexTests :: FilePath -> Spec
rgbHexTests currentDirectory = do
  let rgbHexTestsFile = currentDirectory ++ rgbHexTestsExtension
  describe "Parse Colors from Hex RGB Format" $ do
    it "Should match our list of rgbHexColors" $
      parseColorsFromFile rgbHexTestsFile `shouldReturn` rgbHexColorsList

rgbaHexTests :: FilePath -> Spec
rgbaHexTests currentDirectory = do
  let rgbaHexTestsFile = currentDirectory ++ rgbaHexTestsExtension
  describe "Parse Colors from Hex RGBA Format" $ do
    it "Should match our list of rgbaHexColors" $
      parseColorsFromFile rgbaHexTestsFile `shouldReturn` rgbaHexColorsList

hexAlphaTests :: FilePath -> Spec
hexAlphaTests currentDirectory = do
  let hexAlphaTestsFile = currentDirectory ++ hexAlphaTestsExtension
  describe "Parse Colors from Hex + Alpha Format" $ do
    it "Should match our list of hexAlphaColors" $
      parseColorsFromFile hexAlphaTestsFile `shouldReturn` hexAlphaColorsList

mixFormatInFileTests :: FilePath -> Spec
mixFormatInFileTests currentDirectory = do
  let mixFormatFile = currentDirectory ++ mixFormatExtension
  describe "Parse Colors from different formats in same file" $ do
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

rgbColorsList :: [OWAColor]
rgbColorsList = map colorFromTuple [("color1", 178.0, 224.0, 67.0, 1.0),
                                    ("color2", 0.0, 255.0, 0.0, 1.0),
                                    ("purple", 255.0, 0.0, 255.0, 1.0),
                                    ("black", 0.0, 0.0, 0.0, 1.0),
                                    ("white", 255.0, 255.0, 255.0, 1.0),
                                    ("christmas", 127.758126, 164.6, 0.0, 1.0)]

rgbaColorsList :: [OWAColor]
rgbaColorsList = map colorFromTuple [("aTest1", 245.0, 173.0, 122.0, 0.94),
                                    ("aTest2", 252.0, 253.0, 108.0, 0.91),
                                    ("aTest3", 85.0, 47.0, 81.0, 0.88),
                                    ("aTest4", 6.0, 75.0, 18.0, 0.15),
                                    ("aTest5", 220.0, 120.0, 5.0, 0.30),
                                    ("aTest6", 191.0, 176.0, 226.0, 0.76),
                                    ("aTest7", 118.0, 210.0, 12.0, 0.77),
                                    ("aTest8", 69.0, 111.0, 44.0, 0.08),
                                    ("aTest9", 19.0, 141.0, 167.0, 0.96),
                                    ("aTest10", 14.0, 152.0, 116.0, 0.18)]

rgbHexColorsList :: [OWAColor]
rgbHexColorsList = map colorFromTuple [("aTest1", 222.0, 173.0, 192.0, 1.0),
                                       ("aTest2", 222.0, 203.0, 17.0, 1.0),
                                       ("aTest3", 29.0, 43.0, 195.0, 1.0),
                                       ("aTest4", 244.0, 244.0, 79.0, 1.0),
                                       ("aTest5", 238.0, 244.0, 79.0, 1.0),
                                       ("aTest6", 53.0, 6.0, 80.0, 1.0)]

rgbaHexColorsList :: [OWAColor]
rgbaHexColorsList = map colorFromTuple [("aTest1", 222.0, 173.0, 192.0, 1.0),
                                        ("aTest2", 222.0, 203.0, 17.0, 52.0 / 255.0),
                                        ("aTest3", 29.0, 43.0, 195.0, 165.0 / 255.0),
                                        ("aTest4", 244.0, 244.0, 79.0, 195.0 / 255.0),
                                        ("aTest5", 238.0, 244.0, 79.0, 45.0 / 255.0),
                                        ("aTest6", 53.0, 6.0, 80.0, 152.0 / 255.0)]

hexAlphaColorsList :: [OWAColor]
hexAlphaColorsList = map colorFromTuple [("aTest1", 222.0, 173.0, 192.0, 1.0),
                                       ("aTest2", 222.0, 203.0, 17.0, 0.76),
                                       ("aTest3", 29.0, 43.0, 195.0, 0.56),
                                       ("aTest4", 244.0, 244.0, 79.0, 0.10),
                                       ("aTest5", 238.0, 244.0, 79.0, 0.75),
                                       ("aTest6", 53.0, 6.0, 80.0, 0.98)]

mixFormatColorsList :: [OWAColor]
mixFormatColorsList = map colorFromTuple [("christmas", 127.758126, 164.6, 0.0, 1.0),
                                       ("aTest1", 245.0, 173.0, 122.0, 0.94),
                                       ("hex", 222.0, 173.0, 192.0, 1.0),
                                       ("hexa", 222.0, 173.0, 192.0, 1.0),
                                       ("hexAlpha", 222.0, 173.0, 192.0, 1.0)]

-- TODO:
-- Failure Cases
-- Keywords:
-- Color, Red, Green, Blue, Alpha, Hex
-- Naming a color by a Keyword or other invalid name
-- Putting words instead numbers (line numbers)
-- Invalid combination of keywords
-- Failure to tab
-- Out of bounds (negative and postive) (give a warning)
