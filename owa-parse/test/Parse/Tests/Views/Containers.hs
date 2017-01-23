module Parse.Tests.Views.Containers (
  runContainerViewParseTests 
) where

import Test.Hspec

import Parse.Tests.Utils (shouldReturnRights)
import Parse.Tests.Views.ContainerObjects
import Parse.ViewParser

runContainerViewParseTests :: FilePath -> IO ()
runContainerViewParseTests currentDirectory = do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  hspec $ do
    containerParseTests parseDirectory
    scrollParseTests parseDirectory

containerParseTests :: FilePath -> Spec
containerParseTests parseDirectory = do
  let testFile1 = parseDirectory ++ basicParseExtension
  let testFile2 = parseDirectory ++ nestedParseExtension
  let testFile3 = parseDirectory ++ twoContainersParseExtension
  describe "Parse Views with ContainerView elements" $ do
    context "When there is a single ContainerView" $
      it "Should match the test view" $
        parseViewFromFile testFile1 `shouldReturnRights` basicContainerTest
    
    context "When there are two of the same type of ContainerView" $
      it "Should match the test view" $
        parseViewFromFile testFile2 `shouldReturnRights` nestedContainerTest

    context "When there are two different types of ContainerViews" $
      it "Should match the test view" $
        parseViewFromFile testFile3 `shouldReturnRights` twoContainersTest

scrollParseTests :: FilePath -> Spec
scrollParseTests parseDirectory = do
  let testFile1 = parseDirectory ++ scrollViewDefaultTestExtension
  let testFile2 = parseDirectory ++ scrollViewVerticalTestExtension
  let testFile3 = parseDirectory ++ scrollViewHorizontalTestExtension
  let testFile4 = parseDirectory ++ scrollViewBothTestExtension
  describe "Parse Views with ScrollView elements" $ do
    context "When the scroll view has no direction specified" $
      it "Should match the vertical test view" $
        parseViewFromFile testFile1 `shouldReturnRights` scrollViewDefaultTestView
    
    context "When the scoll view has a vertical direction" $
      it "Should match the vertical test view" $
        parseViewFromFile testFile2 `shouldReturnRights` scrollViewVerticalTestView

    context "When the scroll view has a horizontal direction" $
      it "Should match the horizontal test view" $
        parseViewFromFile testFile3 `shouldReturnRights` scrollViewHorizontalTestView

    context "When the scroll view has both directions" $
      it "Should match the both-direction test view" $
        parseViewFromFile testFile4 `shouldReturnRights` scrollViewBothTestView

parseDirectoryExtension :: String
parseDirectoryExtension = "/test/Parse/Tests/Views/ParseFiles"

basicParseExtension :: String
basicParseExtension = "/basicContainerTest.view"

nestedParseExtension :: String
nestedParseExtension = "/nestedContainerTest.view"

twoContainersParseExtension :: String
twoContainersParseExtension = "/twoContainersTest.view"

scrollViewDefaultTestExtension :: String
scrollViewDefaultTestExtension = "/scrollViewDefaultTest.view"

scrollViewVerticalTestExtension :: String
scrollViewVerticalTestExtension = "/scrollViewVerticalTest.view"

scrollViewHorizontalTestExtension :: String
scrollViewHorizontalTestExtension = "/scrollViewHorizontalTest.view"

scrollViewBothTestExtension :: String
scrollViewBothTestExtension = "/scrollViewBothTest.view"

