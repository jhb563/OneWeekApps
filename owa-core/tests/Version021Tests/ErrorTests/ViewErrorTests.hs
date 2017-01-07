-- This module will test the method:
-- parseViewFromFile :: FilePath -> IO Either [OWAParseError] OWAView
-- which parses a view from a .view file when given a filepath, looking
-- specifically at cases which should return errors

module ViewErrorTests (
  runViewErrorTests
) where

import Test.Hspec

import OWAViewParser
import TestErrors
import TestUtil

runViewErrorTests :: FilePath -> IO ()
runViewErrorTests currentDirectory = hspec $ do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  customViewErrorTests parseDirectory
  containerViewErrorTests parseDirectory
  scrollViewErrorTests parseDirectory

customViewErrorTests :: FilePath -> Spec
customViewErrorTests parseDirectory = do
  let testFile1 = parseDirectory ++ customViewErrorTestExtension1
  let testFile2 = parseDirectory ++ customViewErrorTestExtension2
  let testFile3 = parseDirectory ++ customViewErrorTestExtension3
  let testFile4 = parseDirectory ++ customViewErrorTestExtension4
  describe "Parse view when custom views contain errors" $ do
    context "when the user uses the intended type name instead of \"CustomView\"" $
      it "Should return a ParseError highlighting the inappropriate name" $
        parseViewFromFile testFile1 `shouldMatchError` customError1

    context "when the user does not assign a type to their custom view" $
      it "Should return an ObjectError indicating that type is missing" $
        parseViewFromFile testFile2 `shouldReturnLefts` customError2

    context "when the user does not assign a name to the custom view" $
      it "Should return a Parse Error highlighting the lack of a name" $
        parseViewFromFile testFile3 `shouldMatchError` customError3

    context "when the user tries to have Elements within the custom view" $
      it "Should return a Parse Error hightlighting the Elements element" $
        parseViewFromFile testFile4 `shouldMatchError` customError4

containerViewErrorTests :: FilePath -> Spec
containerViewErrorTests parseDirectory = do
  let testFile1 = parseDirectory ++ containerViewErrorTestExtension1
  let testFile2 = parseDirectory ++ containerViewErrorTestExtension2
  let testFile3 = parseDirectory ++ containerViewErrorTestExtension3
  describe "Parse view when container views contain errors" $ do
    context "when the user uses an inappropriate name instead of \"ContainerView\"" $
      it "Should return a Parse Error highlighting the inappropriate name" $
        parseViewFromFile testFile1 `shouldMatchError` containerError1

    context "when the user uses a lowercase tag \"containerView\"" $
      it "Should return a Parse Error highlighting the lowercase use" $
        parseViewFromFile testFile2 `shouldMatchError` containerError2

    context "when the user uses an errorneous element tag like Text inside a Container" $
      it "Should return a Parse Error highlighting the erroneous element" $
        parseViewFromFile testFile3 `shouldMatchError` containerError3

scrollViewErrorTests :: FilePath -> Spec
scrollViewErrorTests parseDirectory = do
  let testFile1 = parseDirectory ++ scrollViewErrorTestExtension1
  let testFile2 = parseDirectory ++ scrollViewErrorTestExtension2
  let testFile3 = parseDirectory ++ scrollViewErrorTestExtension3
  let testFile4 = parseDirectory ++ scrollViewErrorTestExtension4
  describe "Parse view when scroll views contain errors" $ do
    context "when the user specifies a vertical scroll view but no right constraint" $
      it "Should return an object error highlighting the lack of a right constraint" $
        parseViewFromFile testFile1 `shouldReturnLefts` scrollError1

    context "when the user specifies a vertical scroll view but no left or right constraints" $
      it "Should return an object error highlighting the lack of these constraints" $
        parseViewFromFile testFile2 `shouldReturnLefts` scrollError2

    context "when the user specifies a horizontal scroll view but no top or bottom constraints" $
      it "Should return an object error highlighting the lack of these constraints" $
        parseViewFromFile testFile3 `shouldReturnLefts` scrollError3

    context "when the user uses lowercase for the \"ScrollDirection\" tag." $
      it "Should return a Parse Error hightlighting the scroll direction element" $
        parseViewFromFile testFile4 `shouldMatchError` scrollError4

parseDirectoryExtension :: FilePath
parseDirectoryExtension = "/tests/Version021Tests/ErrorTests/ParseFiles/"

customViewErrorTestExtension1 :: String
customViewErrorTestExtension1 = "customViewError1.view"

customViewErrorTestExtension2 :: String
customViewErrorTestExtension2 = "customViewError2.view"

customViewErrorTestExtension3 :: String
customViewErrorTestExtension3 = "customViewError3.view"

customViewErrorTestExtension4 :: String
customViewErrorTestExtension4 = "customViewError4.view"

containerViewErrorTestExtension1 :: String
containerViewErrorTestExtension1 = "containerViewError1.view"

containerViewErrorTestExtension2 :: String
containerViewErrorTestExtension2 = "containerViewError2.view"

containerViewErrorTestExtension3 :: String
containerViewErrorTestExtension3 = "containerViewError3.view"

scrollViewErrorTestExtension1 :: String
scrollViewErrorTestExtension1 = "scrollViewError1.view"

scrollViewErrorTestExtension2 :: String
scrollViewErrorTestExtension2 = "scrollViewError2.view"

scrollViewErrorTestExtension3 :: String
scrollViewErrorTestExtension3 = "scrollViewError3.view"

scrollViewErrorTestExtension4 :: String
scrollViewErrorTestExtension4 = "scrollViewError4.view"
