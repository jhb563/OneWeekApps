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
objcHeaderFromFonts categoryName fonts = ObjcFile []

-- | 'objcImplementationFromColors' takes a name for the new fonts category, as well
-- as a list of font objects, and returns the structure for the category's
-- implementation file in Objective C
objcImplementationFromFonts :: String -> [OWAFont] -> ObjcFile
objcImplementationFromFonts categoryName fonts = ObjcFile []

