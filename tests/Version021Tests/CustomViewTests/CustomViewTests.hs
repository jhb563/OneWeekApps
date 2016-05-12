-- This module will test the method:
-- parseViewFromFile :: FilePath -> IO Either [OWAParseError] OWAView
-- which parses a view from a .view file when given a filepath, looking
-- at the specific case of parsing custom views.

module CustomViewTests (
  runCustomViewTests
) where

import OWAViewParser
import TestUtil
import TestViews
import Test.Hspec

runCustomViewTests :: FilePath -> IO ()
runCustomViewTests currentDirectory = hspec $ do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  viewParseTests parseDirectory

viewParseTests :: FilePath -> Spec
viewParseTests parseDirectory = do
  let testFile1 = parseDirectory ++ basicParseExtension
  let testFile2 = parseDirectory ++ twoSameParseExtension
  let testFile3 = parseDirectory ++ twoDifferentParseExtension
  describe "Parse Views with CustomView elements" $ do
    context "When there is a single CustomView" $
      it "Should match the test view" $
        parseViewFromFile testFile1 `shouldReturnRights` basicCustomTest
    
    context "When there are two of the same type of CustomView" $
      it "Should match the test view" $
        parseViewFromFile testFile2 `shouldReturnRights` twoSameCustomTest

    context "When there are two different types of CustomViews" $
      it "Should match the test view" $
        parseViewFromFile testFile3 `shouldReturnRights` twoDifferentCustomTest

parseDirectoryExtension :: String
parseDirectoryExtension = "/tests/Version021Tests/CustomViewTests/ParseFiles"

basicParseExtension :: String
basicParseExtension = "/basicCustomTest.view"

twoSameParseExtension :: String
twoSameParseExtension = "/twoSameCustomTest.view"

twoDifferentParseExtension :: String
twoDifferentParseExtension = "/twoDifferentCustomTest.view"
