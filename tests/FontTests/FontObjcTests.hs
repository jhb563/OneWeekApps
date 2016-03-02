-- OWAFontObjc will expose the methods:
-- objcHeaderFromFonts :: String -> [OWAFont] -> ObjcFile
-- objcImplementationFromFonts :: String -> [OWAFont] -> ObjcFile
-- which each take a category name and a list of fonts and return a file structure
-- of Objective C Statements

module FontObjcTests (
  runFontObjcTests
) where

import OWAFont
import OWAFontObjc
import TestFontObjcObjects
import TestFonts
import Test.Hspec

runFontObjcTests = hspec $ do
  fontHeaderTest
  fontImplementationTest

fontHeaderTest :: Spec
fontHeaderTest = describe "Create Header File Structure for Fonts" $ do
  it "Header Structure should match our test" $
    objcHeaderFromFonts fontCategoryName allTestFonts `shouldBe` fontsHeaderFile

fontImplementationTest :: Spec
fontImplementationTest = describe "Create Implementation File Structure for Fonts" $ do
  it "Implementation Structure should match our test" $
    objcImplementationFromFonts fontCategoryName allTestFonts `shouldBe` fontsImplementationFile 

fontCategoryName :: String
fontCategoryName = "MyAppFonts"

