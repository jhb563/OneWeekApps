{-|
Module      : OWAViewObjc
Description : Module for Converting OWAViews to Objective C objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAViewObjc (
  objcHeaderFromView,
  objcImplementationFromView
) where

import ObjcUtil
import OWAAppInfo
import OWAObjcAbSyn
import OWAView

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'objcHeaderFromView' takes the app info and a view and returns
-- the structure for the view's header file.
objcHeaderFromView :: OWAAppInfo -> OWAView -> ObjcFile
objcHeaderFromView appInfo view = ObjcFile []

-- | 'objcImplementationFromView' takes the app info and a view and returns
-- the structure for the view's implementation file.
objcImplementationFromView :: OWAAppInfo -> OWAView -> ObjcFile
objcImplementationFromView appInfo view = ObjcFile []
