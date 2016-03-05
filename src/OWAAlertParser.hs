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

import Data.Maybe
import OWAAlert
import ParseUtil
import qualified Data.Map.Strict as Map
import Text.Parsec
import Text.Parsec.Error
import Text.ParserCombinators.Parsec

type AlertAttr = String
data AlertVal = LocalizedKey 
type AlertAttrMap = Map.Map AlertAttr AlertVal

---------------------------------------------------------------------------
--------------------ENTRY METHODS------------------------------------------
---------------------------------------------------------------------------

-- | 'parseAlertsFromFile' takes a file, reads its contents
-- and returns a list of alerts contained in the file.
parseAlertsFromFile :: FilePath -> IO [OWAAlert]
parseAlertsFromFile fPath = do
  contents <- readFile fPath
  let errorOrAlerts = parseAlertContents contents
  either printErrorAndReturnEmpty (return . catMaybes) errorOrAlerts

parseAlertContents :: String -> Either ParseError [Maybe OWAAlert]
parseAlertContents = parse (many alertParser) ""

---------------------------------------------------------------------------
--------------------PARSERS------------------------------------------------
---------------------------------------------------------------------------

alertParser :: GenParser Char st (Maybe OWAAlert)
alertParser = do
  spaces
  name <- nameParserWithKeyword alertKeyword
  attrs <- many1 alertAttrLine
  let attrMap = Map.fromList attrs
  return (alertFromNameAndAttrMap name attrMap)

alertAttrLine :: GenParser Char st (AlertAttr, AlertVal)
alertAttrLine = do
  string "\t" <|> string "  "
  choice alertAttrParsers

alertAttrParsers :: [GenParser Char st (AlertAttr, AlertVal)]
alertAttrParsers = map (Text.Parsec.try . localizedKeyParserWithKeyword) attributeKeywords

localizedKeyParserWithKeyword :: String -> GenParser Char st (AlertAttr, AlertVal)
localizedKeyParserWithKeyword keyword = do
  string keyword
  char ' '
  localizedKey <- parseLocalizedKey
  endOfLine
  return (keyword, localizedKey)

parseLocalizedKey :: GenParser Char st AlertVal
parseLocalizedKey = do
  

---------------------------------------------------------------------------
--------------------CONSTRUCTING ALERTS------------------------------------
---------------------------------------------------------------------------

alertFromNameAndAttrMap :: String -> AlertAttrMap -> Maybe OWAAlert
alertFromNameAndAttrMap name attrMap = Nothing

---------------------------------------------------------------------------
--------------------ALERT KEYWORDS-----------------------------------------
---------------------------------------------------------------------------

alertKeyword :: String
alertKeyword = "Alert"

attributeKeywords = ["Title",
  "Message",
  "DismissButton",
  "NeutralButton",
  "YesButton",
  "NoButton"]
