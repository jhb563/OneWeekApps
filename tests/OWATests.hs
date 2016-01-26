module Main where

import OWALib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Validate titleString" $ do
    it "titleString should return Running OWA" $ do
      titleString `shouldBe` "Running OWA!"

