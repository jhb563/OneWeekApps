-- OWAColorObjc will expose the methods:
-- objcHeaderFromColors :: String -> [OWAColor] -> ObjcFile
-- objcImplementationFromColors :: String -> [OWAColor] -> ObjcFile
-- which each take a category name and a list of colors and return a file structure
-- of Objective C Statements

module ColorObjcTests (
  runColorObjcTests
) where

import OWAColor
import OWAColorObjc
import OWAObjcAbSyn
import Test.Hspec

runColorObjcTests :: IO ()
runColorObjcTests = hspec $ do
  noColorTest
  oneColorTest
  manyColorsTest

noColorTest :: Spec
noColorTest = do
  describe "Create File Structure with no colors" $ do
    it "Header Structure should match structure with no methods" $
      objcHeaderFromColors noColorCatName [] `shouldBe` emptyHeaderStructure

    it "M Structure should match structure with no methods" $
      objcImplementationFromColors noColorCatName [] `shouldBe` emptyMStructure

oneColorTest :: Spec
oneColorTest = do
  describe "Create File Structure with one color" $ do
    it "Header Structure should match structure with one method" $
      objcHeaderFromColors oneColorCatName oneColorList `shouldBe` oneColorHeaderStructure

    it "M Structure should match structure with one method" $
      objcImplementationFromColors oneColorCatName oneColorList `shouldBe` oneColorMStructure

manyColorsTest :: Spec
manyColorsTest = do
  describe "Create File Structure with many colors" $ do
    it "Header Structure should match structure with many methods" $
      objcHeaderFromColors manyColorCatName manyColorsList `shouldBe` manyColorHeaderStructure

    it "M Structure should match structure with many methods" $
      objcImplementationFromColors manyColorCatName manyColorsList `shouldBe` manyColorMStructure


noColorCatName :: String
noColorCatName = "NoColorCategory"

oneColorCatName :: String
oneColorCatName = "MyOneColorCategory"

manyColorCatName :: String
manyColorCatName = "OWASampleColors"

oneColorList :: [OWAColor]
oneColorList = [colorFromTuple ("christmas", 127.758126, 164.6, 0.0, 1.0)]

manyColorsList :: [OWAColor]
manyColorsList = map colorFromTuple [("aTest1", 245.0, 173.0, 122.0, 0.94),
                                    ("aTest2", 252.0, 253.0, 108.0, 0.91),
                                    ("aTest3", 85.0, 47.0, 81.0, 0.88),
                                    ("aTest4", 6.0, 75.0, 18.0, 0.15),
                                    ("aTest5", 220.0, 120.0, 5.0, 0.30),
                                    ("aTest6", 191.0, 176.0, 226.0, 0.76),
                                    ("aTest7", 118.0, 210.0, 12.0, 0.77),
                                    ("aTest8", 69.0, 111.0, 44.0, 0.08),
                                    ("aTest9", 19.0, 141.0, 167.0, 0.96),
                                    ("aTest10", 14.0, 152.0, 116.0, 0.18)]

emptyHeaderStructure :: ObjcFile
emptyHeaderStructure = ObjcFile []

emptyMStructure :: ObjcFile
emptyMStructure = ObjcFile []

oneColorHeaderStructure :: ObjcFile
oneColorHeaderStructure = ObjcFile []

oneColorMStructure :: ObjcFile
oneColorMStructure = ObjcFile []

manyColorHeaderStructure :: ObjcFile
manyColorHeaderStructure = ObjcFile []

manyColorMStructure :: ObjcFile
manyColorMStructure = ObjcFile []
