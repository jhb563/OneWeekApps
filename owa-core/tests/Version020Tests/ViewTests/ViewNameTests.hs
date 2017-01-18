-- This module will test the method:
-- parseViewFromFile :: FilePath -> IO Either [OWAParseError] OWAView
-- which parses a view from a .view file when given a filepath.

module ViewNameTests (
  runViewNameTests
) where

import Test.Hspec

import Parse.ViewParser
import TestUtil
import TestViews

runViewNameTests :: FilePath -> IO ()
runViewNameTests currentDirectory = hspec $ do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  simpleNameTest parseDirectory
  nameAndTypeTest1 parseDirectory
  nameAndTypeTest2 parseDirectory
  spacedTest parseDirectory
  commentedTest parseDirectory

simpleNameTest :: FilePath -> Spec
simpleNameTest parseDirectory = do
  let testFile = parseDirectory ++ simpleNameExtension
  describe "Parse view with only a name" $
    it "Should return a matching view, inferring the type from the filename" $
      parseViewFromFile testFile `shouldReturnRights` nameTest1

nameAndTypeTest1 :: FilePath -> Spec
nameAndTypeTest1 parseDirectory = do
  let testFile = parseDirectory ++ nameAndTypeExtension1
  describe "Parse view with a name and a type" $
    it "Should return a matching view" $
      parseViewFromFile testFile `shouldReturnRights` nameTest2

nameAndTypeTest2 :: FilePath -> Spec
nameAndTypeTest2 parseDirectory = do
  let testFile = parseDirectory ++ nameAndTypeExtension2
  describe "Parse view with a name and a type" $
    it "Should return a matching view" $
      parseViewFromFile testFile `shouldReturnRights` nameTest3

spacedTest :: FilePath -> Spec
spacedTest parseDirectory = do
  let testFile = parseDirectory ++ spacedExtension
  describe "Parse view where attributes are spaced out more" $
    it "Should return a matching view" $
      parseViewFromFile testFile `shouldReturnRights` nameTest4

commentedTest :: FilePath -> Spec
commentedTest parseDirectory = do
  let testFile = parseDirectory ++ commentedExtension
  describe "Parse view with comments in the file" $
    it "Should return a matching view" $
      parseViewFromFile testFile `shouldReturnRights` nameTest5

parseDirectoryExtension :: String
parseDirectoryExtension = "/tests/Version020Tests/ViewTests/ViewParseFiles"

simpleNameExtension :: String
simpleNameExtension = "/VIANameTestView.view"

nameAndTypeExtension1 :: String
nameAndTypeExtension1 = "/VIANameTestView2.view"

nameAndTypeExtension2 :: String
nameAndTypeExtension2 = "/vianametest3.view"

spacedExtension :: String
spacedExtension = "/VIANameTest4.view"

commentedExtension :: String
commentedExtension = "/VIANameTest5.view"
