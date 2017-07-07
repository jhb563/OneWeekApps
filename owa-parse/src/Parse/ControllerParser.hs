{-|
Module      : Parse.ControllerParser
Description : Module for parsing values from a .controller file into a OWAController model
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Parse.ControllerParser 
  ( parseControllerFromFile ) where

import Model.OWAController (OWAController)
import Model.OWAParseError (OWAParseError)

-------------------------------------------------------------------------------
----------------------ENTRY METHODS--------------------------------------------
-------------------------------------------------------------------------------

-- | 'parseControllerFromFile' takes a file, reads its contents,
-- and returns a list of colors contained in the file.
parseControllerFromFile :: FilePath -> IO (Either [OWAParseError] OWAController)
parseControllerFromFile = undefined
