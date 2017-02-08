-- This module will test the method:
-- parseViewFromFile :: FilePath -> IO Either [OWAParseError] OWAView
-- which parses a view from a .view file when given a filepath, looking
-- specifically at constraints with the parsed views

module Parse.Tests.Views.Constraints (
  runViewConstraintTests
) where

import Test.Hspec

import Parse.Tests.Utils (shouldReturnRights)
import Parse.Tests.Views.Objects
import Parse.ViewParser

runViewConstraintTests :: FilePath -> IO ()
runViewConstraintTests currentDirectory = hspec $ do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  noLayoutTest parseDirectory
  heightWidthTest parseDirectory
  alignTest parseDirectory
  placementTest parseDirectory
  centerTest parseDirectory

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

alignTest :: String -> Spec
alignTest parseDirectory = do
  let testFile1 = parseDirectory ++ alignExtension1
  let testFile2 = parseDirectory ++ alignExtension2
  describe "Parse Layout with alignment constraints" $ do
    context "where views are aligning to the super view, with and without padding" $
      it "Should return a view with proper alignment constraints" $
        parseViewFromFile testFile1 `shouldReturnRights` alignTestView1

    context "where views are aligning to other views, with and without padding" $
      it "Should return a view with proper alignment constraints" $
        parseViewFromFile testFile2 `shouldReturnRights` alignTestView2

placementTest :: String -> Spec
placementTest parseDirectory = do
  let testFile = parseDirectory ++ placementExtension
  describe "Parse Layout with placement constraints" $
    context "where views are placed next to each other with and without padding" $
      it "Should return a view with proper placement constraints" $
        parseViewFromFile testFile `shouldReturnRights` placementTestView

centerTest :: String -> Spec
centerTest parseDirectory = do
  let testFile = parseDirectory ++ centerExtension
  describe "Parse Layout with centering constraints" $
    context "where views are centered to the super view or each other, with and without padding" $
      it "Should return a view with proper centering constraints" $
        parseViewFromFile testFile `shouldReturnRights` centerTestView
      
parseDirectoryExtension :: String
parseDirectoryExtension = "/test/Parse/Tests/Views/ParseFiles"

noLayoutExtension :: String
noLayoutExtension = "/VIAConstraintTest1.view"

heightWidthExtension :: String
heightWidthExtension = "/VIAConstraintTest2.view"

alignExtension1 :: String
alignExtension1 = "/VIAConstraintTest3.view"

alignExtension2 :: String
alignExtension2 = "/VIAConstraintTest4.view"

placementExtension :: String
placementExtension = "/VIAConstraintTest5.view"

centerExtension :: String
centerExtension = "/VIAConstraintTest6.view"
