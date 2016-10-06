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

-- The Swift file consists of:
-- 1. Block comment section at the top. Very similar to other files. 
-- 2. Import of UIKit. Easy. 
-- 2.5 Basic bracketed outline of View
-- 3. Lifecycle method section:
--  a. MARK comment with the title
--  b. Init frame: method
--  c. Init coder: method
--  d. initCommon method
-- 4. Setup Section
--  a. MARK comment
--  b. setupViews
--  c. setupConstraints
-- 5. Lazy Getter Section
--  a. Different getters for each type of view (Label, Button, Field, Image)
