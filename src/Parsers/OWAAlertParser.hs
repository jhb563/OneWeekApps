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
type AlertVal = LocalizedKey 
type AlertAttrMap = Map.Map AlertAttr AlertVal
type AlertParserState = ([String], Bool)

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
parseAlertContents = Text.Parsec.runParser
  (alertParser `endBy` commentOrSpacesParser)
  ([], False)
  ""

---------------------------------------------------------------------------
--------------------PARSERS------------------------------------------------
---------------------------------------------------------------------------

alertParser :: GenParser Char AlertParserState (Maybe OWAAlert)
alertParser = do
  commentOrSpacesParser
  name <- nameParserWithKeyword alertKeyword
  (currentIndentLevel, _) <- getState
  putState (currentIndentLevel, True)
  many $ Text.Parsec.try alertIndentedComment
  attrs <- alertAttrLine `sepEndBy1` many (Text.Parsec.try alertIndentedComment)
  let attrMap = Map.fromList attrs
  return (alertFromNameAndAttrMap name attrMap)

alertAttrLine :: GenParser Char AlertParserState (AlertAttr, AlertVal)
alertAttrLine = do
  (currentIndentLevel, shouldUpdateLevel) <- getState
  string $ concat currentIndentLevel
  if shouldUpdateLevel
    then do 
      newLevel <- readNewIndentationLevel 
      putState (currentIndentLevel ++ [newLevel], False)
      choice alertAttrParsers
    else do
      choice alertAttrParsers  

alertAttrParsers :: [GenParser Char AlertParserState (AlertAttr, AlertVal)]
alertAttrParsers = map (Text.Parsec.try . localizedKeyParserWithKeyword) attributeKeywords

readNewIndentationLevel :: GenParser Char AlertParserState String
readNewIndentationLevel = many (oneOf " \t")

alertIndentedComment :: GenParser Char AlertParserState ()
alertIndentedComment = do
  (currentIndentLevel, shouldUpdateLevel) <- getState
  string $ concat currentIndentLevel
  if shouldUpdateLevel
    then do
      newLevel <- readNewIndentationLevel
      putState (currentIndentLevel ++ [newLevel], False)
      singleTrailingComment
    else do
      singleTrailingComment

---------------------------------------------------------------------------
--------------------CONSTRUCTING ALERTS------------------------------------
---------------------------------------------------------------------------

alertFromNameAndAttrMap :: String -> AlertAttrMap -> Maybe OWAAlert
alertFromNameAndAttrMap name attrMap = do
  title <- case Map.lookup titleKeyword attrMap of
    Just title -> Just title
    _ -> Just ""
  message <- case Map.lookup messageKeyword attrMap of
    Just message -> Just message
    _ -> Just ""
  buttonFormat <- buttonFormatFromAttrMap attrMap
  return OWAAlert {
    alertName = name,
    alertTitle = title,
    alertMessage = message,
    alertButtonFormat = buttonFormat
  } 

buttonFormatFromAttrMap :: AlertAttrMap -> Maybe AlertButtonFormat
buttonFormatFromAttrMap attrMap 
  | Map.member dismissButtonKeyword attrMap = do
    dismissTitle <- Map.lookup dismissButtonKeyword attrMap
    return $ DismissButton dismissTitle
  | Map.member neutralButtonKeyword attrMap = do
    neutralTitle <- Map.lookup neutralButtonKeyword attrMap
    return $ NeutralButton neutralTitle
  | otherwise = do
    yesTitle <- Map.lookup yesButtonKeyword attrMap
    noTitle <- Map.lookup noButtonKeyword attrMap
    return $ YesNoButtons yesTitle noTitle

---------------------------------------------------------------------------
--------------------ALERT KEYWORDS-----------------------------------------
---------------------------------------------------------------------------

alertKeyword :: String
alertKeyword = "Alert"

titleKeyword :: AlertAttr
titleKeyword = "Title"

messageKeyword :: AlertAttr
messageKeyword = "Message"

dismissButtonKeyword :: AlertAttr
dismissButtonKeyword = "DismissButton"

neutralButtonKeyword :: AlertAttr
neutralButtonKeyword = "NeutralButton"

yesButtonKeyword :: AlertAttr
yesButtonKeyword = "YesButton"

noButtonKeyword :: AlertAttr
noButtonKeyword = "NoButton"

attributeKeywords = [titleKeyword,
  messageKeyword,
  dismissButtonKeyword,
  neutralButtonKeyword,
  yesButtonKeyword,
  noButtonKeyword]
