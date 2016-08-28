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
import OWAAppInfo
import OWAColor
import OWAObjcAbSyn
import OWAObjcUtil

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'objcHeaderFromColors' takes the app info,
-- a name for the new colors category, as well
-- as a list of color objects, and returns the structure for the category's
-- header file in Objective C
objcHeaderFromColors :: OWAAppInfo -> [OWAColor] -> ObjcFile
objcHeaderFromColors appInfo colors = ObjcFile 
  [categoryCommentSection appInfo originalColorTypeName categoryName True,
  uiKitImportsSection,
  simpleCategoryInterface category]
    where (categoryName, category) = builderInfo appInfo colors

-- | 'objcImplementationFromColors' takes the app info,
-- a name for the new colors category, as well
-- as a list of color objects, and returns the structure for the category's
-- implementation file in Objective C
objcImplementationFromColors :: OWAAppInfo -> [OWAColor] -> ObjcFile
objcImplementationFromColors appInfo colors = ObjcFile
  [categoryCommentSection appInfo originalColorTypeName categoryName False,
  categoryMImportsSection originalColorTypeName categoryName,
  simpleCategoryImplementation category]
    where (categoryName, category) = builderInfo appInfo colors

builderInfo :: OWAAppInfo -> [OWAColor] -> (String, Category)
builderInfo appInfo colors = (categoryName,
  colorCategoryFromColors categoryName sortedColors)
    where categoryName = appPrefix appInfo ++ "Colors"
          sortedColors = sortBy sortColorsByName colors

--------------------------------------------------------------------------------
--------------------------CATEGORY CONSTRUCTION---------------------------------
--------------------------------------------------------------------------------

colorCategoryFromColors :: String -> [OWAColor] -> Category
colorCategoryFromColors categoryName = categoryFromNamesAndMethodBuilder
  originalColorTypeName categoryName methodForColor 

methodForColor :: OWAColor -> ObjcMethod
methodForColor color = ObjcMethod {
  isStatic = True,
  nameIntro = colorName color,
  returnType = PointerType originalColorTypeName,
  params = [],
  methodBody = [ReturnStatement $ returnExpressionForColor color]
}

returnExpressionForColor :: OWAColor -> ObjcExpression
returnExpressionForColor color = MethodCall (Var originalColorTypeName) colorWithRGBAMethod 
  [FloatLit $ red color,
  FloatLit $ green color,
  FloatLit $ blue color,
  FloatLit $ alpha color]

--------------------------------------------------------------------------------
-------------------------LIBRARY METHOD-----------------------------------------
--------------------------------------------------------------------------------

colorWithRGBAMethod :: CalledMethod 
colorWithRGBAMethod = LibMethod {
  libNameIntro = "colorWith",
  libParams = ["Red", "green", "blue", "alpha"]
}

--------------------------------------------------------------------------------
--------------------------TYPE KEYWORDS-----------------------------------------
--------------------------------------------------------------------------------

originalColorTypeName :: String
originalColorTypeName = "UIColor"

--------------------------------------------------------------------------------
--------------------------SORT HELPER-------------------------------------------
--------------------------------------------------------------------------------

sortColorsByName :: OWAColor -> OWAColor -> Ordering
sortColorsByName color1 color2 = colorName color1 `compare` colorName color2
