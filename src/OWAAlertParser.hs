{-|
Module      : OWAAlertParser
Description : Module for parsing alerts from a .alerts file into OWAAlert models
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAAlertParser (
  parseAlertsFromFile
) where

import OWAAlert

---------------------------------------------------------------------------
--------------------ENTRY METHODS------------------------------------------
---------------------------------------------------------------------------

-- | 'parseAlertsFromFile' takes a file, reads its contents
-- and returns a list of alerts contained in the file.
parseAlertsFromFile :: FilePath -> IO [OWAAlert]
parseAlertsFromFile fPath = return []

