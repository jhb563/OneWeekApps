{-|
Module      : OWAStringsObjc
Description : Module for Converting OWALocalizedStringSets to Objective C objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAStringsObjc (
  objcStringsFileFromStringSets
) where

import OWAAppInfo
import OWALocalizedStringSet
import OWAObjcAbSyn

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

objcStringsFileFromStringSets :: OWAAppInfo -> [OWALocalizedStringSet] -> ObjcFile
objcStringsFileFromStringSets appInfo stringSets = ObjcFile []
