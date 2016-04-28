-- This module will test the method:
-- parseViewFromFile :: FilePath -> IO Either [OWAParseError] OWAView
-- which parses a view from a .view file when given a filepath, looking
-- specifically at constraints with the parsed views

module ViewConstraintTests (
  runViewConstraintTests
) where

import OWAViewParser
import TestUtil
import TestViews
import Test.Hspec

runViewConstraintTests :: FilePath -> IO ()
runViewConstraintTests currentDirectory = hspec $ do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  noLayoutTest parseDirectory
  heightWidthTest parseDirectory

noLayoutTest :: String -> Spec
noLayoutTest parseDirectory = do
  let testFile = parseDirectory ++ noLayoutExtension
  describe "Parse empty Layout section" $
    it "Should return a view with no constraints" $
      parseViewFromFile testFile `shouldReturnRights` noLayoutTestView

heightWidthTest :: String -> Spec
heightWidthTest parseDirectory = do
  let testFile = parseDirectory ++ heightWidthExtension
  describe "Parse Layout with heights and widths" $
    context "as numbers, matching other views, and matching other views with padding" $
      it "Should return a view with proper height and width constraints" $
        parseViewFromFile testFile `shouldReturnRights` heightWidthTestView

parseDirectoryExtension :: String
parseDirectoryExtension = "/tests/Version020Tests/ViewTests/ViewParseFiles"

noLayoutExtension :: String
noLayoutExtension = "/VIAConstraintTest1.view"

heightWidthExtension :: String
heightWidthExtension = "/VIAConstraintTest2.view"
