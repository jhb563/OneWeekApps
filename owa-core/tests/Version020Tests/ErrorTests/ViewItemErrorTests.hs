-- This module will test the method:
-- parseViewFromFile :: FilePath -> IO Either [OWAParseError] OWAView
-- which parses a view from a .view file when given a filepath, looking
-- specifically at cases which should return ObjectErrors

module ViewItemErrorTests (
  runViewItemErrorTests
) where

import Test.Hspec

import Parse.ViewParser
import TestErrors
import TestUtil

runViewItemErrorTests :: FilePath -> IO ()
runViewItemErrorTests currentDirectory = hspec $ do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  labelItemErrorTests parseDirectory
  buttonItemErrorTests parseDirectory
  imageItemErrorTests parseDirectory
  combinedItemErrorTests parseDirectory

labelItemErrorTests :: FilePath -> Spec
labelItemErrorTests parseDirectory = do
  let testFile = parseDirectory ++ labelErrorExtension
  describe "Parse view when a label is missing the Text attribute" $
    it "Should return an ObjectError indicating the missing field" $
      parseViewFromFile testFile `shouldReturnLefts` testLabelErrors

buttonItemErrorTests :: FilePath -> Spec
buttonItemErrorTests parseDirectory = do
  let testFile = parseDirectory ++ buttonErrorExtension
  describe "Parse view when a button is missing the Text attribute" $
    it "Should return an ObjectError indicating the missing field" $
      parseViewFromFile testFile `shouldReturnLefts` testButtonErrors

imageItemErrorTests :: FilePath -> Spec
imageItemErrorTests parseDirectory = do
  let testFile = parseDirectory ++ imageErrorExtension
  describe "Parse view when an image is missing the ImageSrc attribute" $
    it "Should return an ObjectError indicating the missing field" $
      parseViewFromFile testFile `shouldReturnLefts` testImageErrors

combinedItemErrorTests :: FilePath -> Spec
combinedItemErrorTests parseDirectory = do
  let testFile = parseDirectory ++ combinedErrorExtension
  describe "Parse view when multiple elements are missing required values" $
    it "Should return multiple object errors, one for each malformed view" $
      parseViewFromFile testFile `shouldReturnLefts` testCombinedErrors

parseDirectoryExtension :: FilePath
parseDirectoryExtension = "/tests/Version020Tests/ErrorTests/ParseFiles/"

labelErrorExtension :: String
labelErrorExtension = "/LabelItemError.view"

buttonErrorExtension :: String
buttonErrorExtension = "/ButtonItemError.view"

imageErrorExtension :: String
imageErrorExtension = "/ImageItemError.view"

combinedErrorExtension :: String
combinedErrorExtension = "/CombinedError.view"
