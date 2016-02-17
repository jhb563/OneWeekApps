{-|
Module      : OWAColorObjc
Description : Module for Converting OWAColors to Objective C objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAColorObjc (
  objcHeaderFromColors,
  objcImplementationFromColors
) where

import OWAColor
import OWAObjcAbSyn

objcHeaderFromColors :: String -> [OWAColor] -> ObjcFile
objcHeaderFromColors categoryName colors = ObjcFile 
  [commentSection True categoryName,
   headerImportSection,
   CategoryInterfaceSection $ colorCategoryFromColors categoryName colors]

objcImplementationFromColors :: String -> [OWAColor] -> ObjcFile
objcImplementationFromColors categoryName colors = ObjcFile
  [commentSection False categoryName,
   mImportSection categoryName,
   CategoryImplementationSection $ colorCategoryFromColors categoryName colors]

commentSection :: Bool -> String -> FileSection
commentSection isHeader categoryName = BlockCommentSection ["",
                                              originalColorTypeName ++ ('+':categoryName) ++ ending,
                                              "MySampleApp",
                                              "",
                                              "Created By James Bowen 2/16/2016",
                                              "Copyright (c) 2016 One Week Apps. All Rights Reserved",
                                              ""]
                                where ending = if isHeader then ".h" else ".m"

headerImportSection :: FileSection
headerImportSection = ImportsSection [ModuleImport "UIKit"]

mImportSection :: String -> FileSection
mImportSection categoryName = ImportsSection [FileImport fileName]
              where fileName = originalColorTypeName ++ ('+':categoryName) ++ ".h"

colorCategoryFromColors :: String -> [OWAColor] -> Category
colorCategoryFromColors categoryName colors = Category {
  originalTypeName = originalColorTypeName,
  categoryName = categoryName,
  categoryMethods = map methodForColor colors
}

methodForColor :: OWAColor -> ObjcMethod
methodForColor color = ObjcMethod {
  isStatic = True,
  nameIntro = colorName color,
  returnType = PointerType "UIColor",
  params = [],
  methodBody = [ReturnStatement $ returnExpressionForColor color]
}

returnExpressionForColor :: OWAColor -> ObjcExpression
returnExpressionForColor color = MethodCall (Var "UIColor") colorWithRGBAMethod [FloatLit $ red color,
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

originalColorTypeName :: String
originalColorTypeName = "UIColor"
