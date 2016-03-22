{-|
Module      : OWAAppInfoParser
Description : Module for parsing app info from a app.info file
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAAppInfoParser (
  parseAppInfoFromFile
) where

import OWAAppInfo
import OWAParseError

parseAppInfoFromFile :: FilePath -> IO (Either [OWAParseError] OWAAppInfo)
parseAppInfoFromFile currentDirectory = return $ Right OWAAppInfo {
  appName = "",
  authorName = "",
  dateCreatedString = "",
  companyName = Nothing
}
