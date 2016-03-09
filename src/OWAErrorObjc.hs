{-|
Module      : OWAErrorObjc
Description : Module for Converting OWAErrors to Objective C objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAErrorObjc (
  objcHeaderFromErrors,
  objcImplementationFromErrors
) where

import OWAError
import OWAObjcAbSyn

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'objcHeaderFromErrors' takes a name for the new errors category, as well
-- as a list of error objects, and returns the structure for the category's
-- header file in Objective C
objcHeaderFromErrors :: String -> [OWAError] -> ObjcFile
objcHeaderFromErrors categoryName errors = ObjcFile []

-- | 'objcImplementationFromErrors' takes a name for the new errors category, as well
-- as a list of error objects, and returns the structure for the category's
-- implementation file in Objective C
objcImplementationFromErrors :: String -> [OWAError] -> ObjcFile
objcImplementationFromErrors categoryName errors = ObjcFile []
