{-|
Module      : Swift.ModelConverter
Description : Module for Converting OWAModels to Swift objects
Copyright   : (c) James Bowen, 2017
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Swift.ModelConverter (
  swiftFileFromModel
) where

import Model.OWAAppInfo
import Model.OWAModel
import Swift.AbSyn

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'swiftFileFromModel' takes the app info,
-- and a model object, and returns the structure for the model's
-- header file in Swift
swiftFileFromModel :: OWAAppInfo -> OWAModel -> SwiftFile
swiftFileFromModel _ _ = SwiftFile []
