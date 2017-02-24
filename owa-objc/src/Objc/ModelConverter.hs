{-|
Module      : Objc.ModelConverter
Description : Module for Converting OWAModels to Objective C objects
Copyright   : (c) James Bowen, 2017
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Objc.ModelConverter (
  objcHeaderFromModel,
  objcImplementationFromModel
) where

import Model.OWAAppInfo
import Model.OWAModel
import Objc.AbSyn

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'objcHeaderFromModels' takes the app info,
-- and a model object, and returns the structure for the model's
-- header file in Objective C
objcHeaderFromModel :: OWAAppInfo -> OWAModel -> ObjcFile
objcHeaderFromModel _ _ = ObjcFile []

-- | 'objcImplementationFromModels' takes the app info,
-- and a model object, and returns the structure for the model's
-- implementation file in Objective C
objcImplementationFromModel :: OWAAppInfo -> OWAModel -> ObjcFile
objcImplementationFromModel _ _ = ObjcFile []
