-- Here we are testing the same parsing methods tested in Version 0.1.0 parsing tests,
-- only now the files being parsed have comments in them.

module CommentTests (
  runCommentTests
) where

import OWAAlertParser
import OWAColorParser
import OWAErrorParser
import OWAFontParser
import TestUtil
import Test.Hspec

runCommentTests :: FilePath -> IO ()
runCommentTests currentDirectory = hspec $ do
  let testDirectory = currentDirectory ++ "/tests/Version015Tests/CommentTests/CommentParseFiles"
  alertCommentTests testDirectory
  colorCommentTests testDirectory
  errorCommentTests testDirectory
  fontCommentTests testDirectory

alertCommentTests :: FilePath -> Spec 
alertCommentTests testDirectory = do
  let alertCommentsTestFile = testDirectory ++ alertCommentsFileExtension
  describe "Parse alerts with comments" $
    context "when the comments exist between alert specification" $
      it "Should successfully parse, returning a non-empty list" $
        shouldReturnWithoutErrors $ parseAlertsFromFile alertCommentsTestFile

colorCommentTests :: FilePath -> Spec 
colorCommentTests testDirectory = do
  let colorCommentsTestFile = testDirectory ++ colorCommentsFileExtension
  describe "Parse colors with comments" $
    context "when commented lines exist among the attributes for colors" $
      it "Should successfully parse, returning a non-empty list" $
        shouldReturnWithoutErrors $ parseColorsFromFile colorCommentsTestFile
      
errorCommentTests :: FilePath -> Spec 
errorCommentTests testDirectory = do
  let errorCommentsTestFile = testDirectory ++ errorCommentsFileExtension
  describe "Parse errors with comments" $
    context "when comments come after item and attribute lines" $
      it "Should successfully parse, returning a non-empty list" $
        shouldReturnWithoutErrors $ parseErrorsFromFile errorCommentsTestFile

fontCommentTests :: FilePath -> Spec 
fontCommentTests testDirectory = do
  let fontCommentsTestFile = testDirectory ++ fontCommentsFileExtension
  describe "Parse fonts with comments" $
    it "Should successfully parse, returning a non-empty list" $
      shouldReturnWithoutErrors $ parseFontsFromFile fontCommentsTestFile

alertCommentsFileExtension :: String
alertCommentsFileExtension = "/comments.alerts"

colorCommentsFileExtension :: String
colorCommentsFileExtension = "/comments.colors"

errorCommentsFileExtension :: String
errorCommentsFileExtension = "/comments.errors"

fontCommentsFileExtension :: String
fontCommentsFileExtension = "/comments.fonts"
