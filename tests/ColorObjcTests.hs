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
noColorTest = describe "Create File Structure with no colors" $ do
    it "Header Structure should match structure with no methods" $
      objcHeaderFromColors noColorCatName [] `shouldBe` emptyHeaderStructure

    it "M Structure should match structure with no methods" $
      objcImplementationFromColors noColorCatName [] `shouldBe` emptyMStructure

oneColorTest :: Spec
oneColorTest = describe "Create File Structure with one color" $ do
    it "Header Structure should match structure with one method" $
      objcHeaderFromColors oneColorCatName oneColorList `shouldBe` oneColorHeaderStructure

    it "M Structure should match structure with one method" $
      objcImplementationFromColors oneColorCatName oneColorList `shouldBe` oneColorMStructure

manyColorsTest :: Spec
manyColorsTest = describe "Create File Structure with many colors" $ do
    it "Header Structure should match structure with many methods" $
      objcHeaderFromColors manyColorCatName manyColorsList `shouldBe` manyColorHeaderStructure

    it "M Structure should match structure with many methods" $
      objcImplementationFromColors manyColorCatName manyColorsList `shouldBe` manyColorMStructure

commentSection :: Bool -> String -> FileSection
commentSection isHeader catName = BlockCommentSection ["",
                                              "UIColor+" ++ catName ++ ending,
                                              "MySampleApp",
                                              "",
                                              "Created By James Bowen 2/16/2016",
                                              "Copyright (c) 2016 One Week Apps. All Rights Reserved",
                                              ""]
                                where ending = if isHeader then ".h" else ".m"

headerImportSection :: FileSection
headerImportSection = ImportsSection [ModuleImport "UIKit"]

mImportSection :: String -> FileSection
mImportSection catName = ImportsSection [FileImport $ "UIColor+" ++ catName ++ ".h"]

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

noColorCategory :: Category
noColorCategory = Category {
  originalTypeName = "UIColor",
  categoryName = noColorCatName,
  categoryMethods = []
}

oneColorCateogry :: Category
oneColorCateogry = Category {
  originalTypeName = "UIColor",
  categoryName = oneColorCatName,
  categoryMethods = [ObjcMethod {
    isStatic = True,
    nameIntro = colorName $ head oneColorList,
    returnType = PointerType "UIColor",
    params = [],
    methodBody = [ReturnStatement $ methodBodyExprForColor (head oneColorList)]
  }]
}

manyColorCategory :: Category
manyColorCategory = Category {
  originalTypeName = "UIColor",
  categoryName = manyColorCatName,
  categoryMethods = map (\color -> ObjcMethod {
    isStatic = True,
    nameIntro = colorName color,
    returnType = PointerType "UIColor",
    params = [],
    methodBody = [ReturnStatement $ methodBodyExprForColor color]
  }) manyColorsList
}

emptyHeaderStructure :: ObjcFile
emptyHeaderStructure = ObjcFile [commentSection True noColorCatName,
                                 headerImportSection,
                                 CategoryInterfaceSection noColorCategory]

emptyMStructure :: ObjcFile
emptyMStructure = ObjcFile [commentSection False noColorCatName,
                            mImportSection noColorCatName,
                            CategoryImplementationSection noColorCategory]

oneColorHeaderStructure :: ObjcFile
oneColorHeaderStructure = ObjcFile [commentSection True oneColorCatName,
                                    headerImportSection,
                                    CategoryInterfaceSection oneColorCateogry]

oneColorMStructure :: ObjcFile
oneColorMStructure = ObjcFile [commentSection False oneColorCatName,
                               mImportSection oneColorCatName,
                               CategoryImplementationSection oneColorCateogry]

manyColorHeaderStructure :: ObjcFile
manyColorHeaderStructure = ObjcFile [commentSection True manyColorCatName,
                                     headerImportSection,
                                     CategoryInterfaceSection manyColorCategory]

manyColorMStructure :: ObjcFile
manyColorMStructure = ObjcFile [commentSection False manyColorCatName,
                                mImportSection manyColorCatName,
                                CategoryImplementationSection manyColorCategory]

methodBodyExprForColor :: OWAColor -> ObjcExpression
methodBodyExprForColor color = MethodCall (Var "UIColor") colorWithRGBAMethod [FloatLit $ red color,
                                                                              FloatLit $ green color,
                                                                              FloatLit $ blue color,
                                                                              FloatLit $ alpha color]

colorWithRGBAMethod :: ObjcMethod
colorWithRGBAMethod = ObjcMethod {
  isStatic = True,
  nameIntro = "colorWith",
  returnType = PointerType "UIColor",
  params = [ParamDef {
    paramTitle = "Red",
    paramType = SimpleType "CGFloat",
    paramName = "red"
  }, ParamDef {
    paramTitle = "green",
    paramType = SimpleType "CGFloat",
    paramName = "green"
  }, ParamDef {
    paramTitle = "blue",
    paramType = SimpleType "CGFloat",
    paramName = "blue"
  }, ParamDef {
    paramTitle = "alpha",
    paramType = SimpleType "CGFloat",
    paramName = "alpha"
  }],
  methodBody = []
}