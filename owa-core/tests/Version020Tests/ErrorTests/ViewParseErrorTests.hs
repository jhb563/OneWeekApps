-- This module will test the method:
-- parseViewFromFile :: FilePath -> IO Either [OWAParseError] OWAView
-- which parses a view from a .view file when given a filepath, looking
-- specifically at cases which should return ParseErrors

module ViewParseErrorTests (
  runViewParseErrorTests
) where

import Test.Hspec

import Parse.ViewParser
import TestErrors
import TestUtil

runViewParseErrorTests :: FilePath -> IO ()
runViewParseErrorTests currentDirectory = hspec $ do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  viewErrorTests parseDirectory
  elementsTest parseDirectory
  layoutTest parseDirectory
 
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

parseDirectoryExtension :: FilePath
parseDirectoryExtension = "/tests/Version020Tests/ErrorTests/ParseFiles/"

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
