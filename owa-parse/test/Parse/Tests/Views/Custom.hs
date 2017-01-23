module Parse.Tests.Views.Custom (
  runCustomViewParseTests
) where

import Test.Hspec

import Parse.Tests.Utils (shouldReturnRights)
import Parse.Tests.Views.CustomObjects
import Parse.ViewParser

runCustomViewParseTests :: FilePath -> IO ()
runCustomViewParseTests currentDirectory = do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  hspec $ customParseTests parseDirectory

customParseTests :: FilePath -> Spec
customParseTests parseDirectory = do
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
parseDirectoryExtension = "/test/Parse/Tests/Views/ParseFiles"

basicParseExtension :: String
basicParseExtension = "/basicCustomTest.view"

twoSameParseExtension :: String
twoSameParseExtension = "/twoSameCustomTest.view"

twoDifferentParseExtension :: String
twoDifferentParseExtension = "/twoDifferentCustomTest.view"
