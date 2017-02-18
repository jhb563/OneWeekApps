module Parse.Tests.Models.Basic 
  ( runModelParseTests )
  where

import Test.Hspec

import Parse.ModelParser
import Parse.Tests.Models.Objects
import Parse.Tests.Utils (shouldReturnRights)

runModelParseTests :: FilePath -> IO ()
runModelParseTests _ = print "Model Parse Tests Not Activated Yet"

-- Different name until we activate these tests.
runModelParseTests' :: FilePath -> IO ()
runModelParseTests' currentDirectory = hspec $ do
  let parseDirectory = currentDirectory ++ parseDirectoryExtension
  basicTest parseDirectory
  customTest parseDirectory
  optionalsTest parseDirectory
  arraysTest parseDirectory
  mapsTest parseDirectory
  completeTest parseDirectory

basicTest :: String -> Spec
basicTest parseDirectory = do
  let testFile = parseDirectory ++ parseExtension1
  describe "Parse a basic model file" $
    it "Should return a matching model" $
      parseModelFromFile testFile `shouldReturnRights` basicModel

customTest :: String -> Spec
customTest parseDirectory = do
  let testFile = parseDirectory ++ parseExtension2
  describe "Parse a model file with custom models" $
    it "Should return a matching model" $
      parseModelFromFile testFile `shouldReturnRights` customModel

optionalsTest :: String -> Spec
optionalsTest parseDirectory = do
  let testFile = parseDirectory ++ parseExtension3
  describe "Parse a model file with optionals" $
    it "Should return a matching model" $
      parseModelFromFile testFile `shouldReturnRights` optionalsModel

arraysTest :: String -> Spec
arraysTest parseDirectory = do
  let testFile = parseDirectory ++ parseExtension4
  describe "Parse a model file with array types" $
    it "Should return a matching model" $
      parseModelFromFile testFile `shouldReturnRights` arraysModel

mapsTest :: String -> Spec
mapsTest parseDirectory = do
  let testFile = parseDirectory ++ parseExtension5
  describe "Parse a model file with map types" $
    it "Should return a matching model" $
      parseModelFromFile testFile `shouldReturnRights` mapsModel

completeTest :: String -> Spec
completeTest parseDirectory = do
  let testFile = parseDirectory ++ parseExtension6
  describe "Parse a model file with a wide variety of types" $
    it "Should return a matching model" $
      parseModelFromFile testFile `shouldReturnRights` completeModel

parseDirectoryExtension :: String
parseDirectoryExtension = "/test/Parse/Tests/Models/ParseFiles"

parseExtension1 :: String
parseExtension1 = "/basicTest.model"

parseExtension2 :: String
parseExtension2 = "/customTest.model"

parseExtension3 :: String
parseExtension3 = "/OptionalsModel.model"

parseExtension4 :: String
parseExtension4 = "/ArrayObject.model"

parseExtension5 :: String
parseExtension5 = "/MapObject.model"

parseExtension6 :: String
parseExtension6 = "/MyCompleteModel.model"
