-- This module will test the method:
-- parseViewFromFile :: FilePath -> IO Either [OWAParseError] OWAView
-- which parses a view from a .view file when given a filepath, looking
-- specifically at elements within the view are parsed.

module ViewElementTests (
  runViewElementTests
) where

import Test.Hspec

import Parse.OWAViewParser
import TestUtil
import TestViews

runViewElementTests :: FilePath -> IO ()
runViewElementTests currentDirectory = hspec $ do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  viewElementTest1 parseDirectory
  viewElementTest2 parseDirectory
  viewElementTest3 parseDirectory

viewElementTest1 :: String -> Spec
viewElementTest1 parseDirectory = do
  let testFile = parseDirectory ++ testExtension1
  describe "Parse elements of a view when there is one of each type of element" $
    it "Should return a matching view" $
      parseViewFromFile testFile `shouldReturnRights` elementTest1

viewElementTest2 :: String -> Spec
viewElementTest2 parseDirectory = do
  let testFile = parseDirectory ++ testExtension2
  describe "Parse elements of a view when there are two of each type of element" $
    it "Should return a matching view" $
      parseViewFromFile testFile `shouldReturnRights` elementTest2

viewElementTest3 :: String -> Spec
viewElementTest3 parseDirectory = do
  let testFile = parseDirectory ++ testExtension3
  describe "Parse elements of a view with some more attribute combinations" $
    it "Should return a matching view" $
      parseViewFromFile testFile `shouldReturnRights` elementTest3

parseDirectoryExtension :: String
parseDirectoryExtension = "/tests/Version020Tests/ViewTests/ViewParseFiles"

testExtension1 :: String
testExtension1 = "/VIAElementTest1.view"

testExtension2 :: String
testExtension2 = "/VIAElementTest2.view"

testExtension3 :: String
testExtension3 = "/VIAElementTest3.view"
