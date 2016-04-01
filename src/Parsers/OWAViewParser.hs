{-|
Module      : OWAViewParser
Description : Module for parsing a view from a .view file into a OWAView model
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAViewParser (
  parseViewFromFile
) where

import OWAParseError
import OWAView

-------------------------------------------------------------------------------
----------------------ENTRY METHODS--------------------------------------------
-------------------------------------------------------------------------------

-- | 'parseViewFromFile' takes a file, reads its contents,
-- and returns a view parsed from the file, or a list of errors encountered.
parseViewFromFile :: FilePath -> IO (Either [OWAParseError] OWAView)
parseViewFromFile fPath = return $ Right OWAView {
  viewName = "",
  viewType = "",
  subviews = [],
  constraints = []
}
