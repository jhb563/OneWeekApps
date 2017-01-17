module Parse.Tests.Utils 
  ( shouldReturnRights )
  where

import Test.Hspec

-- Unwraps result of parsing and expects that we have a full list of objects
-- matching the given objects.
shouldReturnRights :: (Show a, Show b, Eq b) => IO (Either [a] b) -> b -> Expectation
shouldReturnRights returned expected = do
  result <- returned
  case result of
    Left errors -> fail ("Parse Returned Errors: " ++ show errors)
    Right xs -> xs `shouldBe` expected
