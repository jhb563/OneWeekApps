{-|
Module      : OWAViewSwift
Description : Module for Converting OWAViews to Swift objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAViewSwift (
  swiftFileFromView
) where

import OWAAppInfo
import OWAView
import OWASwiftAbSyn

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'swiftFileFromView' takes the app info,
-- and a list of color objects and returns the structure for the extension's
-- Swift file
swiftFileFromView :: OWAAppInfo -> OWAView -> SwiftFile
swiftFileFromView appInfo view = SwiftFile []
