{-|
Module      : OWAErrorParser
Description : Module for parsing errors from a .errors file into OWAError models
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAErrorParser (
  parseErrorsFromFile
) where

import Data.Maybe
import OWAError
import ParseUtil
import qualified Data.Map.Strict as Map
import Text.Parsec
import Text.Parsec.Error
import Text.ParserCombinators.Parsec

---------------------------------------------------------------------------
--------------------ENTRY METHODS------------------------------------------
---------------------------------------------------------------------------

-- | 'parseErrorsFromFile' takes a file, reads its contents, and returns
-- a list of errors contained in the file.
parseErrorsFromFile :: FilePath -> IO [OWAError]
parseErrorsFromFile fPath = return []
