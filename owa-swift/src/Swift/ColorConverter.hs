{-|
Module      : Swift.ColorConverter
Description : Module for Converting OWAColors to Swift objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Swift.ColorConverter (
  swiftExtensionFromColors
) where

import Data.List

import Model.OWAAppInfo
import Model.OWAColor
import Swift.AbSyn
import Swift.Utils

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
  listSectionForColors (sort colors)]
  where filename = colorExtensionFileName appInfo

--------------------------------------------------------------------------------
--------------------------EXTENSION CONSTRUCTION--------------------------------
--------------------------------------------------------------------------------

listSectionForColors :: [OWAColor] -> FileSection
listSectionForColors colors = ExtensionSection 
  originalColorTypeName
  maybeMethodSection
  where maybeMethodSection = if null colors
          then []
          else [MethodImplementationListSection Nothing $ map methodForColor colors] 

methodForColor :: OWAColor -> SwiftMethod
methodForColor color = SwiftMethod
  False
  ["class"]
  (colorName color)
  (Just $ SimpleType originalColorTypeName)
  []
  [ReturnStatement $ returnExpressionForColor color]

returnExpressionForColor :: OWAColor -> SwiftExpression
returnExpressionForColor color = MethodCall 
  Nothing
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
  [Just "red", Just "green", Just "blue", Just "alpha"]

--------------------------------------------------------------------------------
--------------------------TYPE KEYWORDS-----------------------------------------
--------------------------------------------------------------------------------

originalColorTypeName :: String
originalColorTypeName = "UIColor"

colorExtensionFileName :: OWAAppInfo -> String
colorExtensionFileName appInfo = originalColorTypeName ++
  ('+' : appPrefix appInfo ++ "Colors.swift")
