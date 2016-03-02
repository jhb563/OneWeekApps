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

import OWAFont
import OWAObjcAbSyn

-- | 'objcHeaderFromFonts' takes a name for the new fonts category, as well
-- as a list of font objects, and returns the structure for the category's
-- header file in Objective C
objcHeaderFromFonts :: String -> [OWAFont] -> ObjcFile
objcHeaderFromFonts categoryName fonts = ObjcFile 
  [commentSection True categoryName,
  headerImportSection,
  CategoryInterfaceSection $ fontCategoryFromFonts categoryName fonts]

-- | 'objcImplementationFromFonts' takes a name for the new fonts category, as well
-- as a list of font objects, and returns the structure for the category's
-- implementation file in Objective C
objcImplementationFromFonts :: String -> [OWAFont] -> ObjcFile
objcImplementationFromFonts categoryName fonts = ObjcFile
  [commentSection False categoryName,
  mImportSection categoryName,
  CategoryImplementationSection $ fontCategoryFromFonts categoryName fonts]

commentSection :: Bool -> String -> FileSection
commentSection isHeader categoryName = BlockCommentSection 
  ["",
  originalFontTypeName ++ ('+':categoryName) ++ ending,
  "MySampleApp",
  "",
  "Created By James Bowen 2/29/2016",
  "Copyright (c) 2016 One Week Apps. All Rights Reserved",
  ""]
  where ending = if isHeader then ".h" else ".m"

headerImportSection :: FileSection
headerImportSection = ImportsSection [ModuleImport "UIKit"]

mImportSection :: String -> FileSection
mImportSection categoryName = ImportsSection [FileImport fileName]
              where fileName = originalFontTypeName ++ ('+':categoryName) ++ ".h"

fontCategoryFromFonts :: String -> [OWAFont] -> Category
fontCategoryFromFonts catName fonts = Category {
  originalTypeName = originalFontTypeName,
  categoryName = catName,
  categoryMethods = map methodForFont fonts
}

methodForFont :: OWAFont -> ObjcMethod
methodForFont font = ObjcMethod {
  isStatic = True,
  nameIntro = fontName font,
  returnType = PointerType "UIFont",
  params = [],
  methodBody = [ReturnStatement $ returnExpressionForFont font]
}

returnExpressionForFont :: OWAFont -> ObjcExpression
returnExpressionForFont font = MethodCall (Var "UIFont") fontWithNameMethod 
  [StringLit $ fullNameForFont font,
  FloatLit $ fontSize font]

fullNameForFont :: OWAFont -> String
fullNameForFont font = case fontStyles font of
  [] -> fontFamily font
  styles -> fontFamily font ++ ('-':styleList)
    where styleList = foldl (\str style -> str ++ show style) "" styles

fontWithNameMethod :: ObjcMethod
fontWithNameMethod = ObjcMethod {
  isStatic = True,
  nameIntro = "fontWith",
  returnType = PointerType "UIFont",
  params = 
    [ParamDef {
      paramTitle = "Name",
      paramType = PointerType "NSString",
      paramName = "name"
    },
    ParamDef {
      paramTitle = "size",
      paramType = SimpleType "CGFloat",
      paramName = "size"
    }],
  methodBody = []
}

originalFontTypeName :: String
originalFontTypeName = "UIFont"

