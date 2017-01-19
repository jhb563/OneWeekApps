module Parse.Tests.Utils 
  ( shouldReturnRights 
  , shouldReturnLefts
  , shouldReturnWithoutErrors 
  , shouldMatchError )
  where

import Text.Parsec.Error
import Text.Parsec.Pos

import Model.OWAParseError
import Test.Hspec

-- Unwraps result of parsing and expects that we have a full list of objects
-- matching the given objects.
shouldReturnRights :: (Show a, Show b, Eq b) => IO (Either [a] b) -> b -> Expectation
shouldReturnRights returned expected = do
  result <- returned
  case result of
    Left errors -> fail ("Parse Returned Errors: " ++ show errors)
    Right xs -> xs `shouldBe` expected

-- Unwraps result of parsing and expects that we have a full list of failures 
-- matching the given error objects.
shouldReturnLefts :: (Show a, Eq a) => IO (Either a b) -> a -> Expectation
shouldReturnLefts returned expected = do
  result <- returned
  case result of
    Right _ -> fail "Parse Returned Completed objects"
    Left xs -> xs `shouldBe` expected

shouldReturnWithoutErrors :: Show b => IO (Either [a] [b]) -> Expectation
shouldReturnWithoutErrors wrappedVals = do
  result <- wrappedVals
  case result of
    Left _ -> fail "Parse Returned Errors"
    Right xs -> xs `shouldSatisfy` (not . null)

shouldMatchError :: IO (Either [OWAParseError] b) -> SourcePos -> Expectation
shouldMatchError returned srcPos = do
  result <- returned
  case result of
    Right _ -> fail "Parse Returned Completed Objects"
    Left [ParsecError parseError] ->
      errorPos parseError `shouldBe` srcPos
    _ -> fail "Incorrect number or format of errors"
