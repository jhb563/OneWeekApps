{-|
Module      : OWAColorSwift
Description : Module for Converting OWAColors to Swift objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAColorSwift (
  swiftExtensionFromColors
) where

import OWAAppInfo
import OWAColor
import OWASwiftAbSyn

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'swiftExtensionFromColors' takes the app info,
-- and a list of color objects and returns the structure for the extension's
-- Swift file
swiftExtensionFromColors :: OWAAppInfo -> [OWAColor] -> SwiftFile
swiftExtensionFromColors appInfo colors = SwiftFile
  [extensionCommentSection appInfo originalColorTypeName categoryName,
  uiKitImportSection,
  listSectionForColors colors]

--------------------------------------------------------------------------------
--------------------------EXTENSION CONSTRUCTION--------------------------------
--------------------------------------------------------------------------------

listSectionForColors :: [OWAColor] -> FileSection
listSectionForColors = ExtensionSection 
  originalColorTypeName
  (map methodForColor colors)

methodForColor :: OWAColor -> SwiftMethod
methodForColor color = SwiftMethod
  True
  colorName color,
  returnType = originalColorTypeName,
  params = [],
  methodBody = [ReturnStatement $ returnExpressionForColor color]

returnExpressionForColor :: OWAColor -> SwiftExpression
returnExpressionForColor color = MethodCall 
  colorWithRGBAMethod
  [FloatLit $ red color,
  FloatLit $ green color,
  FloatLit $ blue color,
  FloatLit $ alpha color]

--------------------------------------------------------------------------------
--------------------------LIBRARY METHOD----------------------------------------
--------------------------------------------------------------------------------

colorWithRGBAMethod :: CalledMethod
colorWithRGBAMethod = LibMethod 
  "UIColor"
  ["red", "green", "blue", "alpha"]

--------------------------------------------------------------------------------
--------------------------TYPE KEYWORDS-----------------------------------------
--------------------------------------------------------------------------------

originalColorTypeName :: String
originalColorTypeName = "UIColor"
