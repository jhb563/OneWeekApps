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
  emptyLayoutTest parseDirectory

emptyLayoutTest :: String -> Spec
emptyLayoutTest parseDirectory = do
  let testFile = parseDirectory ++ emptyLayoutExtension
  describe "Parse empty Layout section" $
    it "Should return a view with no constraints" $
      parseViewFromFile testFile `shouldReturnRights` emptyLayoutTestView

parseDirectoryExtension :: String
parseDirectoryExtension = "/tests/Version020Tests/ViewTests/ViewParseFiles"

emptyLayoutExtension :: String
emptyLayoutExtension = "/VIAConstraintTest1.view"
