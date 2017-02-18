{-|
Module      : Parse.ColorParser
Description : Module for parsing values from a .model file into OWAModel models
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Parse.ModelParser
  ( parseModelFromFile) where

import Model.OWAModel
import Model.OWAParseError

-------------------------------------------------------------------------------
----------------------ENTRY METHODS--------------------------------------------
-------------------------------------------------------------------------------

-- | 'parseModelFromFile' takes a file, reads its contents, and returns the
-- model represented by the file, or a list of possible errors.
parseModelFromFile :: FilePath -> IO (Either [OWAParseError] OWAModel)
parseModelFromFile = undefined
