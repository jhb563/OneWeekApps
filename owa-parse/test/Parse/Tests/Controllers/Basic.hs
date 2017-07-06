module Parse.Tests.Controllers.Basic
  ( runControllerParseTests )
  where

import Test.Hspec

import Parse.ControllerParser (parseControllerFromFile)
import Parse.Tests.Controllers.Objects 
  (basicController, multiChoiceController, normalController, textController)
import Parse.Tests.Utils (shouldReturnRights)

runControllerParseTests :: FilePath -> IO ()
runControllerParseTests currentDirectory = hspec $ do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  basicTest parseDirectory
  multiChoiceTest parseDirectory
  normalTest parseDirectory
  textTest parseDirectory

basicTest :: String -> Spec
basicTest parseDirectory = do
  let testFile = parseDirectory ++ basicParseExtension
  describe "Parse a basic controller file" $
    it "Should return a matching controller" $
      parseModelFromFile testFile `shouldReturnRights` basicController

multiChoiceTest :: String -> Spec
multiChoiceTest parseDirectory = do
  let testFile = parseDirectory ++ multiChoiceParseExtension
  describe "Parse a multiChoice controller file" $
    it "Should return a matching controller" $
      parseModelFromFile testFile `shouldReturnRights` multiChoiceController

normalTest :: String -> Spec
normalTest parseDirectory = do
  let testFile = parseDirectory ++ normalParseExtension
  describe "Parse a normal controller file" $
    it "Should return a matching controller" $
      parseModelFromFile testFile `shouldReturnRights` normalController

textTest :: String -> Spec
textTest parseDirectory = do
  let testFile = parseDirectory ++ textParseExtension
  describe "Parse a text controller file" $
    it "Should return a matching controller" $
      parseModelFromFile testFile `shouldReturnRights` textController

parseDirectoryExtension :: String
parseDirectoryExtension = "/test/Parse/Tests/Controllers/ParseFiles"

basicParseExtension :: String
basicParseExtension = "/BasicController.controller"

multiChoiceParseExtension :: String
multiChoiceParseExtension = "/MultiChoiceController.controller"

normalParseExtension :: String
normalParseExtension = "/NormalController.controller"

textParseExtension :: String
textParseExtension = "/TextController.controller"
