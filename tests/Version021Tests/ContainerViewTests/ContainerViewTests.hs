-- This module will test the method:
-- parseViewFromFile :: FilePath -> IO Either [OWAParseError] OWAView
-- which parses a view from a .view file when given a filepath, looking
-- at the specific case of parsing container views.
-- 
-- It will also test the methods:
-- objcHeaderFromView :: OWAAppInfo -> OWAView -> ObjcFile
-- objcImplementationFromView :: OWAAppInfo -> OWAView -> ObjcFile
-- from OWAViewObjc follwed by:
-- printStructureToFile :: ObjcFile -> Doc
-- in OWAObjcPrint, again testing the container views case

module ContainerViewTests (
  runContainerViewTests
) where

import OWAViewParser
import TestUtil
import TestContainerViews
import Test.Hspec

runContainerViewTests :: FilePath -> IO ()
runContainerViewTests currentDirectory = hspec $ do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  viewParseTests parseDirectory

viewParseTests :: FilePath -> Spec
viewParseTests parseDirectory = do
  let testFile1 = parseDirectory ++ basicParseExtension
  let testFile2 = parseDirectory ++ nestedParseExtension
  let testFile3 = parseDirectory ++ twoContainersParseExtension
  describe "Parse Views with ContainerView elements" $ do
    context "When there is a single ContainerView" $
      it "Should match the test view" $
        parseViewFromFile testFile1 `shouldReturnRights` basicContainerTest
    
    context "When there are two of the same type of CustomView" $
      it "Should match the test view" $
        parseViewFromFile testFile2 `shouldReturnRights` nestedContainerTest

    context "When there are two different types of CustomViews" $
      it "Should match the test view" $
        parseViewFromFile testFile3 `shouldReturnRights` twoContainersTest

parseDirectoryExtension :: String
parseDirectoryExtension = "/tests/Version021Tests/ContainerViewTests/ParseFiles"

basicParseExtension :: String
basicParseExtension = "/basicContainerTest.view"

nestedParseExtension :: String
nestedParseExtension = "/nestedContainerTest.view"

twoContainersParseExtension :: String
twoContainersParseExtension = "/twoContainersTest.view"
