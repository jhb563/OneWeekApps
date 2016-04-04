-- This module will test the method:
-- parseViewFromFile :: FilePath -> IO Either [OWAParseError] OWAView
-- which parses a view from a .view file when given a filepath, looking
-- specifically at elements within the view are parsed.

module ViewElementTests (
  runViewElementTests
) where

import OWAViewParser
import TestUtil
import TestViews
import Test.Hspec

runViewElementTests :: FilePath -> IO ()
runViewElementTests currentDirectory = hspec $ do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  elementTest1 parseDirectory
  elementTest2 parseDirectory
  elementTest3 parseDirectory

elementTest1 :: Spec
elementTest1 parseDirectory = do
  let testFile = parseDirectory ++ testExtension1
  describe "Parse elements of a view when there is one of each type of element" $
    it "Should return a matching view" $
      parseViewFromFile testFile `shouldReturnRights` elementTest1

elementTest2 :: Spec
elementTest2 parseDirectory = do
  let testFile = parseDirectory ++ testExtension2
  describe "Parse elements of a view when there are two of each type of element" $
    it "Should return a matching view" $
      parseViewFromFile testFile `shouldReturnRights` elementTest2

elementTest3 :: Spec
elementTest3 parseDirectory = do
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
