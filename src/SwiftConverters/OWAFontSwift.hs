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
import OWAAppInfo
import OWAFont
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
  listSectionForFonts sortedFonts]
  where filename = fontExtensionFileName appInfo
        sortedFonts = sortBy sortFontsByName fonts

--------------------------------------------------------------------------------
--------------------------EXTENSION CONSTRUCTION--------------------------------
--------------------------------------------------------------------------------

listSectionForFonts :: [OWAFont] -> FileSection
listSectionForFonts fonts = ExtensionSection 
  originalFontTypeName
  maybeMethodSection
  where maybeMethodSection = if null fonts
          then []
          else [MethodImplementationListSection $ map methodForFont fonts] 

methodForFont :: OWAFont -> SwiftMethod
methodForFont font = SwiftMethod
  True
  (fontName font)
  (OptionalType originalFontTypeName)
  []
  [ReturnStatement $ returnExpressionForFont font]

returnExpressionForFont :: OWAFont -> SwiftExpression
returnExpressionForFont font = MethodCall 
  fontWithFamilySizeMethod
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

fontWithFamilySizeMethod :: CalledMethod
fontWithFamilySizeMethod = LibMethod
  "UIFont"
  ["name", "size"]

--------------------------------------------------------------------------------
--------------------------TYPE KEYWORDS-----------------------------------------
--------------------------------------------------------------------------------

originalFontTypeName :: String
originalFontTypeName = "UIFont"

fontExtensionFileName :: OWAAppInfo -> String
fontExtensionFileName appInfo = originalFontTypeName ++
  ('+' : appPrefix appInfo ++ "Fonts.swift")

--------------------------------------------------------------------------------
--------------------------SORT HELPER-------------------------------------------
--------------------------------------------------------------------------------

sortFontsByName :: OWAFont -> OWAFont -> Ordering
sortFontsByName font1 font2 = fontName font1 `compare` fontName font2
