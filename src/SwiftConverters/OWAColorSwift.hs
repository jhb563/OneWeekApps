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

import Data.List
import OWAAppInfo
import OWAColor
import OWASwiftAbSyn
import OWASwiftUtil

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'swiftExtensionFromColors' takes the app info,
-- and a list of color objects and returns the structure for the extension's
-- Swift file
swiftExtensionFromColors :: OWAAppInfo -> [OWAColor] -> SwiftFile
swiftExtensionFromColors appInfo colors = SwiftFile
  [extensionCommentSection filename appInfo,
  uiKitImportSection,
  listSectionForColors sortedColors]
  where filename = colorExtensionFileName appInfo
        sortedColors = sortBy sortColorsByName colors

--------------------------------------------------------------------------------
--------------------------EXTENSION CONSTRUCTION--------------------------------
--------------------------------------------------------------------------------

listSectionForColors :: [OWAColor] -> FileSection
listSectionForColors colors = ExtensionSection 
  originalColorTypeName
  maybeMethodSection
  where maybeMethodSection = if null colors
          then []
          else [MethodImplementationListSection $ map methodForColor colors] 

methodForColor :: OWAColor -> SwiftMethod
methodForColor color = SwiftMethod
  True
  (colorName color)
  (SimpleType originalColorTypeName)
  []
  [ReturnStatement $ returnExpressionForColor color]

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

colorExtensionFileName :: OWAAppInfo -> String
colorExtensionFileName appInfo = originalColorTypeName ++
  ('+' : appPrefix appInfo ++ "Colors.swift")

--------------------------------------------------------------------------------
--------------------------SORT HELPER-------------------------------------------
--------------------------------------------------------------------------------

sortColorsByName :: OWAColor -> OWAColor -> Ordering
sortColorsByName color1 color2 = colorName color1 `compare` colorName color2
