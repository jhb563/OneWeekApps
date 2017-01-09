{-|
Module      : OWAFontSwift
Description : Module for Converting OWAFonts to Swift objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAFontSwift (
  swiftExtensionFromFonts
) where

import Data.List

import Model.OWAAppInfo
import Model.OWAFont
import OWASwiftAbSyn
import OWASwiftUtil

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'swiftExtensionFromFonts' takes the app info,
-- and a list of font objects and returns the structure for the extension's
-- Swift file
swiftExtensionFromFonts :: OWAAppInfo -> [OWAFont] -> SwiftFile
swiftExtensionFromFonts appInfo fonts = SwiftFile
  [extensionCommentSection filename appInfo,
  uiKitImportSection,
  listSectionForFonts (sort fonts)]
  where filename = fontExtensionFileName appInfo

--------------------------------------------------------------------------------
--------------------------EXTENSION CONSTRUCTION--------------------------------
--------------------------------------------------------------------------------

listSectionForFonts :: [OWAFont] -> FileSection
listSectionForFonts fonts = ExtensionSection 
  originalFontTypeName
  maybeMethodSection
  where maybeMethodSection = if null fonts
          then []
          else [MethodImplementationListSection Nothing $ map methodForFont fonts] 

methodForFont :: OWAFont -> SwiftMethod
methodForFont font = SwiftMethod
  False
  ["class"]
  (fontName font)
  (Just $ OptionalType (SimpleType originalFontTypeName))
  []
  [ReturnStatement $ returnExpressionForFont font]

returnExpressionForFont :: OWAFont -> SwiftExpression
returnExpressionForFont font = MethodCall 
  Nothing
  fontWithFamilySizeMethod
  [StringLit $ fullNameForFont font,
  FloatLit $ fontSize font]

--------------------------------------------------------------------------------
--------------------------LIBRARY METHOD----------------------------------------
--------------------------------------------------------------------------------

fontWithFamilySizeMethod :: CalledMethod
fontWithFamilySizeMethod = LibMethod
  "UIFont"
  [Just "name", Just "size"]

--------------------------------------------------------------------------------
--------------------------TYPE KEYWORDS-----------------------------------------
--------------------------------------------------------------------------------

originalFontTypeName :: String
originalFontTypeName = "UIFont"

fontExtensionFileName :: OWAAppInfo -> String
fontExtensionFileName appInfo = originalFontTypeName ++
  ('+' : appPrefix appInfo ++ "Fonts.swift")
