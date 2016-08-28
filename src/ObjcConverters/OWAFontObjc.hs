{-|
Module      : OWAFontObjc
Description : Module for Converting OWAFonts to Objective C objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAFontObjc (
  objcHeaderFromFonts,
  objcImplementationFromFonts
) where

import Data.List
import OWAAppInfo
import OWAFont
import OWAObjcAbSyn
import OWAObjcUtil

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'objcHeaderFromFonts' takes the app info, 
-- a name for the new fonts category, as well
-- as a list of font objects, and returns the structure for the category's
-- header file in Objective C
objcHeaderFromFonts :: OWAAppInfo -> [OWAFont] -> ObjcFile
objcHeaderFromFonts appInfo fonts = ObjcFile 
  [categoryCommentSection appInfo originalFontTypeName categoryName True,
  uiKitImportsSection,
  simpleCategoryInterface category] 
    where (categoryName, category) = builderInfo appInfo fonts

-- | 'objcImplementationFromFonts' takes the app info,
-- a name for the new fonts category, as well
-- as a list of font objects, and returns the structure for the category's
-- implementation file in Objective C
objcImplementationFromFonts :: OWAAppInfo -> [OWAFont] -> ObjcFile
objcImplementationFromFonts appInfo fonts = ObjcFile
  [categoryCommentSection appInfo originalFontTypeName categoryName False,
  categoryMImportsSection originalFontTypeName categoryName,
  simpleCategoryImplementation category]
    where (categoryName, category) = builderInfo appInfo fonts

builderInfo :: OWAAppInfo -> [OWAFont] -> (String, Category)
builderInfo appInfo fonts = (categoryName,
  fontCategoryFromFonts categoryName sortedFonts)
    where categoryName = appPrefix appInfo ++ "Fonts"
          sortedFonts = sortBy sortFontsByName fonts

--------------------------------------------------------------------------------
--------------------------CATEGORY CONSTRUCTION---------------------------------
--------------------------------------------------------------------------------

fontCategoryFromFonts :: String -> [OWAFont] -> Category
fontCategoryFromFonts categoryName = categoryFromNamesAndMethodBuilder
  originalFontTypeName categoryName methodForFont

methodForFont :: OWAFont -> ObjcMethod
methodForFont font = ObjcMethod {
  isStatic = True,
  nameIntro = fontName font,
  returnType = PointerType originalFontTypeName,
  params = [],
  methodBody = [ReturnStatement $ returnExpressionForFont font]
}

returnExpressionForFont :: OWAFont -> ObjcExpression
returnExpressionForFont font = MethodCall (Var originalFontTypeName) fontWithNameMethod 
  [StringLit $ fullNameForFont font,
  FloatLit $ fontSize font]

fullNameForFont :: OWAFont -> String
fullNameForFont font = case fontStyles font of
  [] -> fontFamily font
  styles -> fontFamily font ++ ('-':styleList)
    where styleList = foldl (\str style -> str ++ show style) "" styles

--------------------------------------------------------------------------------
--------------------------LIBRARY METHOD----------------------------------------
--------------------------------------------------------------------------------

fontWithNameMethod :: CalledMethod 
fontWithNameMethod = LibMethod {
  libNameIntro = "fontWith",
  libParams = ["Name", "size"]
}

--------------------------------------------------------------------------------
--------------------------TYPE KEYWORDS-----------------------------------------
--------------------------------------------------------------------------------

originalFontTypeName :: String
originalFontTypeName = "UIFont"

--------------------------------------------------------------------------------
--------------------------SORT HELPER-------------------------------------------
--------------------------------------------------------------------------------

sortFontsByName :: OWAFont -> OWAFont -> Ordering
sortFontsByName font1 font2 = fontName font1 `compare` fontName font2
