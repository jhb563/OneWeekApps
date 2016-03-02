-- OWAFontObjc will expose the methods:
-- objcHeaderFromFonts :: String -> [OWAFont] -> ObjcFile
-- objcImplementationFromFonts :: String -> [OWAFont] -> ObjcFile
-- which each take a category name and a list of fonts and return a file structure
-- of Objective C Statements

module FontObjcTests (
  runFontObjcTests
) where

import OWAFontObjc
import TestFontObjcObjects
import TestFonts
import Test.Hspec

runFontObjcTests :: IO ()
runFontObjcTests = hspec $ do
  fontHeaderTest
  fontImplementationTest
  noMethodsTest

fontHeaderTest :: Spec
fontHeaderTest = describe "Create Header File Structure for Fonts" $
  it "Header Structure should match our test" $
    objcHeaderFromFonts fontCategoryName allTestFonts `shouldBe` fontsHeaderFile

fontImplementationTest :: Spec
fontImplementationTest = describe "Create Implementation File Structure for Fonts" $
  it "Implementation Structure should match our test" $
    objcImplementationFromFonts fontCategoryName allTestFonts `shouldBe` fontsImplementationFile 

noMethodsTest :: Spec
noMethodsTest = describe "Create Files with no methods" $ do
  it "Header structure should match empty category header" $
    objcHeaderFromFonts fontCategoryName [] `shouldBe` emptyFontsHeaderFile

  it "Implementation structure should match empty category implementation" $
    objcImplementationFromFonts fontCategoryName [] `shouldBe` emptyFontsImplementationFile

fontCategoryName :: String
fontCategoryName = "MyAppFonts"

