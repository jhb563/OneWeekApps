{-|
Module      : OWAStringsParser
Description : Module for parsing strings from a .strings file into a string map
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAStringsParser (
  parseStringsFromFile
) where

import OWAParseError
import qualified Data.Map.Strict as Map

type StringMap = Map.Map String String

---------------------------------------------------------------------------
--------------------ENTRY METHODS------------------------------------------
---------------------------------------------------------------------------

parseStringsFromFile :: FilePath -> IO (Either [OWAParseError] StringMap)
parseStringsFromFile fPath = return $ Right Map.empty
