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

import Data.List
import ObjcUtil
import OWAColor
import OWAObjcAbSyn

-- | 'objcHeaderFromColors' takes a name for the new colors category, as well
-- as a list of color objects, and returns the structure for the category's
-- header file in Objective C
objcHeaderFromColors :: String -> [OWAColor] -> ObjcFile
objcHeaderFromColors categoryName colors = ObjcFile 
  [categoryCommentSection originalColorTypeName categoryName True,
  uiKitImportsSection,
  CategoryInterfaceSection $ colorCategoryFromColors categoryName sortedColors]
    where sortedColors = sortBy sortColorsByName colors 

-- | 'objcImplementationFromColors' takes a name for the new colors category, as well
-- as a list of color objects, and returns the structure for the category's
-- implementation file in Objective C
objcImplementationFromColors :: String -> [OWAColor] -> ObjcFile
objcImplementationFromColors categoryName colors = ObjcFile
  [categoryCommentSection originalColorTypeName categoryName False,
  categoryMImportsSection originalColorTypeName categoryName,
  CategoryImplementationSection $ colorCategoryFromColors categoryName sortedColors]
    where sortedColors = sortBy sortColorsByName colors

colorCategoryFromColors :: String -> [OWAColor] -> Category
colorCategoryFromColors categoryName = categoryFromNamesAndMethodBuilder
  originalColorTypeName categoryName methodForColor 

methodForColor :: OWAColor -> ObjcMethod
methodForColor color = ObjcMethod {
  isStatic = True,
  nameIntro = colorName color,
  returnType = PointerType "UIColor",
  params = [],
  methodBody = [ReturnStatement $ returnExpressionForColor color]
}

returnExpressionForColor :: OWAColor -> ObjcExpression
returnExpressionForColor color = MethodCall (Var "UIColor") colorWithRGBAMethod 
  [FloatLit $ red color,
  FloatLit $ green color,
  FloatLit $ blue color,
  FloatLit $ alpha color]

colorWithRGBAMethod :: ObjcMethod
colorWithRGBAMethod = ObjcMethod {
  isStatic = True,
  nameIntro = "colorWith",
  returnType = PointerType "UIColor",
  params = 
    [ParamDef {
      paramTitle = "Red",
      paramType = SimpleType "CGFloat",
      paramName = "red"
    }, 
    ParamDef {
      paramTitle = "green",
      paramType = SimpleType "CGFloat",
      paramName = "green"
    }, 
    ParamDef {
      paramTitle = "blue",
      paramType = SimpleType "CGFloat",
      paramName = "blue"
    }, 
    ParamDef {
      paramTitle = "alpha",
      paramType = SimpleType "CGFloat",
      paramName = "alpha"
    }],
  methodBody = []
}

originalColorTypeName :: String
originalColorTypeName = "UIColor"

sortColorsByName :: OWAColor -> OWAColor -> Ordering
sortColorsByName color1 color2 = colorName color1 `compare` colorName color2
