-- This module will test the method:
-- parseViewFromFile :: FilePath -> IO Either [OWAParseError] OWAView
-- which parses a view from a .view file when given a filepath, looking
-- specifically at cases which return some kind of error.

module Parse.Tests.Views.Failure (
  runViewFailureTests
) where

import Test.Hspec

import Parse.Tests.Utils (shouldMatchError, shouldReturnLefts)
import Parse.Tests.Views.Errors
import Parse.ViewParser

runViewFailureTests :: FilePath -> IO ()
runViewFailureTests currentDirectory = hspec $ do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  viewErrorTests parseDirectory
  elementsTest parseDirectory
  layoutTest parseDirectory
  labelItemErrorTests parseDirectory
  buttonItemErrorTests parseDirectory
  imageItemErrorTests parseDirectory
  combinedItemErrorTests parseDirectory
  imageButtonErrorTests parseDirectory
  customViewErrorTests parseDirectory
  containerViewErrorTests parseDirectory
  scrollViewErrorTests parseDirectory
 
viewErrorTests :: FilePath -> Spec
viewErrorTests parseDirectory = do
  let testFile1 = parseDirectory ++ uppercaseViewNameFailExtension
  let testFile2 = parseDirectory ++ lowercaseViewTagFailExtension
  let testFile3 = parseDirectory ++ invalidViewTagExtension
  describe "Parse incorrectly formatted views" $ do
    context "when an uppercase name is used for the view" $
      it "Should return a parse error highlighting the error location" $
        parseViewFromFile testFile1 `shouldMatchError` uppercaseViewNameFailError
    
    context "when a lowercase view tag is used to start the view" $
      it "Should return a parse error highlighting the error location" $
        parseViewFromFile testFile2 `shouldMatchError` lowercaseViewTagFailError

    context "when an invalid tag is used inside the view" $
      it "Should return a parse error highlighting the error location" $
        parseViewFromFile testFile3 `shouldMatchError` invalidViewTagError

elementsTest :: FilePath -> Spec
elementsTest parseDirectory = do
  let testFile1 = parseDirectory ++ invalidElementsTagExtension
  let testFile2 = parseDirectory ++ uppercaseElementNameFailExtension
  let testFile3 = parseDirectory ++ placeholderLabelFailExtension
  describe "Parse views with incorrectly formatted elements" $ do
    context "when an invalid tag is used as an element" $
      it "Should return a parse error highlighting the error location" $
        parseViewFromFile testFile1 `shouldMatchError` invalidElementsTagError

    context "when an uppercase name is used for an element" $
      it "Should return a parse error highlighting the error location" $
        parseViewFromFile testFile2 `shouldMatchError` uppercaseElementNameFailError

    context "when a placeholder tag is used on a Label element" $
      it "Should return a parse error highlighting the error location" $
        parseViewFromFile testFile3 `shouldMatchError` placeholderLabelFailError

layoutTest :: FilePath -> Spec
layoutTest parseDirectory = do
  let testFile2 = parseDirectory ++ noIndentLayoutExtension
  let testFile3 = parseDirectory ++ invalidLayoutTagExtension
  let testFile4 = parseDirectory ++ lowercaseLayoutTagExtension
  let testFile5 = parseDirectory ++ emptyHeightTagExtension
  let testFile6 = parseDirectory ++ emptyAboveTagExtension
  let testFile7 = parseDirectory ++ noViewToRightExtension
  let testFile8 = parseDirectory ++ incorrectCenterExtension
  describe "Parse views with incorrectly formatted layouts" $ do
    context "when layout lines are not indented" $
      it "Should return a parse error highlighting the error location" $
        parseViewFromFile testFile2 `shouldMatchError` noIndentLayoutError

    context "when an invalid tag is used in the layout section" $
      it "Should return a parse error highlighting the error location" $
        parseViewFromFile testFile3 `shouldMatchError` invalidLayoutTagError
      
    context "when a lowercase layout tag is used" $
      it "Should return a parse error highlighting the error location" $
        parseViewFromFile testFile4 `shouldMatchError` lowercaseLayoutTagError

    context "when a height tag is left empty" $
      it "Should return a parse error highlighting the error location" $
        parseViewFromFile testFile5 `shouldMatchError` emptyHeightTagError

    context "when an above tag is left empty" $
      it "Should return a parse error highlighting the error location" $
        parseViewFromFile testFile6 `shouldMatchError` emptyAboveTagError

    context "when a ToRightOf tag has only a dimension and no attached view" $
      it "Should return a parse error highlighting the error location" $
        parseViewFromFile testFile7 `shouldMatchError` noViewToRightError

    context "when Center is used as a tag instead of CenterX or CenterY" $
      it "Should return a parse error highlighting the error location" $
        parseViewFromFile testFile8 `shouldMatchError` incorrectCenterError

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

imageButtonErrorTests :: FilePath -> Spec
imageButtonErrorTests parseDirectory = do
  let testFile1 = parseDirectory ++ imageButtonExtension1
  let testFile2 = parseDirectory ++ imageButtonExtension2
  describe "Parse view files where buttons use image source tag incorrectly" $ do

    context "When the image file name is improperly not in quotations" $ 
      it "Should return a parse error highlighting the improper name" $
        parseViewFromFile testFile1 `shouldMatchError` imageButtonMissingQuotesError

    context "When the wrong tag is used instead of ImageSrc" $
      it "Should return a parse error highlighting the improper tag" $
        parseViewFromFile testFile2 `shouldMatchError` imageButtonWrongTagNameError

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
parseDirectoryExtension = "/test/Parse/Tests/Views/ParseFiles"

uppercaseViewNameFailExtension :: String
uppercaseViewNameFailExtension = "/uppercaseViewNameFail.view"

lowercaseViewTagFailExtension :: String
lowercaseViewTagFailExtension = "/lowercaseViewTagFail.view"

invalidViewTagExtension :: String
invalidViewTagExtension = "/invalidViewTag.view"

invalidElementsTagExtension :: String
invalidElementsTagExtension = "/invalidElementsTag.view"

uppercaseElementNameFailExtension :: String
uppercaseElementNameFailExtension = "/uppercaseElementNameFail.view"

placeholderLabelFailExtension :: String
placeholderLabelFailExtension = "/placeholderLabelFail.view"

noIndentLayoutExtension :: String
noIndentLayoutExtension = "/noIndentLayout.view"

invalidLayoutTagExtension :: String
invalidLayoutTagExtension = "/invalidLayoutTag.view"

lowercaseLayoutTagExtension :: String
lowercaseLayoutTagExtension = "/lowercaseLayoutTag.view"

emptyHeightTagExtension :: String
emptyHeightTagExtension = "/emptyHeightTag.view"

emptyAboveTagExtension :: String
emptyAboveTagExtension = "/emptyAboveTag.view"

noViewToRightExtension :: String
noViewToRightExtension = "/noViewToRight.view"

incorrectCenterExtension :: String
incorrectCenterExtension = "/incorrectCenter.view"

labelErrorExtension :: String
labelErrorExtension = "/LabelItemError.view"

buttonErrorExtension :: String
buttonErrorExtension = "/ButtonItemError.view"

imageErrorExtension :: String
imageErrorExtension = "/ImageItemError.view"

combinedErrorExtension :: String
combinedErrorExtension = "/CombinedError.view"

imageButtonExtension1 :: String
imageButtonExtension1 = "/imageButtonError1.view"

imageButtonExtension2 :: String
imageButtonExtension2 = "/imageButtonError2.view"

customViewErrorTestExtension1 :: String
customViewErrorTestExtension1 = "/customViewError1.view"

customViewErrorTestExtension2 :: String
customViewErrorTestExtension2 = "/customViewError2.view"

customViewErrorTestExtension3 :: String
customViewErrorTestExtension3 = "/customViewError3.view"

customViewErrorTestExtension4 :: String
customViewErrorTestExtension4 = "/customViewError4.view"

containerViewErrorTestExtension1 :: String
containerViewErrorTestExtension1 = "/containerViewError1.view"

containerViewErrorTestExtension2 :: String
containerViewErrorTestExtension2 = "/containerViewError2.view"

containerViewErrorTestExtension3 :: String
containerViewErrorTestExtension3 = "/containerViewError3.view"

scrollViewErrorTestExtension1 :: String
scrollViewErrorTestExtension1 = "/scrollViewError1.view"

scrollViewErrorTestExtension2 :: String
scrollViewErrorTestExtension2 = "/scrollViewError2.view"

scrollViewErrorTestExtension3 :: String
scrollViewErrorTestExtension3 = "/scrollViewError3.view"

scrollViewErrorTestExtension4 :: String
scrollViewErrorTestExtension4 = "/scrollViewError4.view"
