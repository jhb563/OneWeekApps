{-|
Module      : Swift.ModelConverter
Description : Module for Converting OWAModels to Swift objects
Copyright   : (c) James Bowen, 2017
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Swift.ModelConverter (
  swiftHeaderFromModel,
  swiftImplementationFromModel
) where

import Model.OWAAppInfo
import Model.OWAModel
import Swift.AbSyn

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'swiftHeaderFromModels' takes the app info,
-- and a model object, and returns the structure for the model's
-- header file in Objective C
swiftHeaderFromModel :: OWAAppInfo -> OWAModel -> SwiftFile
swiftHeaderFromModel _ _ = SwiftFile []

-- | 'swiftImplementationFromModels' takes the app info,
-- and a model object, and returns the structure for the model's
-- implementation file in Objective C
swiftImplementationFromModel :: OWAAppInfo -> OWAModel -> SwiftFile
swiftImplementationFromModel _ _ = SwiftFile []
