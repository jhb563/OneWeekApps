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
-- and a list of color objects and returns the structure for the extension's
-- Swift file
swiftExtensionFromFonts :: OWAAppInfo -> [OWAFont] -> SwiftFile
swiftExtensionFromFonts appInfo colors = SwiftFile []
