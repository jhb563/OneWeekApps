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

import Data.Either
import Data.List
import Data.Maybe
import OWAAlert
import OWAParseError
import ParseUtil
import qualified Data.Map.Strict as Map
import Text.Parsec
import Text.Parsec.Error
import Text.ParserCombinators.Parsec

type AlertAttr = String
type AlertVal = LocalizedKey 
type AlertAttrMap = Map.Map AlertAttr AlertVal

---------------------------------------------------------------------------
--------------------ENTRY METHODS------------------------------------------
---------------------------------------------------------------------------

-- | 'parseAlertsFromFile' takes a file, reads its contents
-- and returns a list of alerts contained in the file.
parseAlertsFromFile :: FilePath -> IO (Either [OWAParseError] [OWAAlert])
parseAlertsFromFile fPath = do
  contents <- readFile fPath
  let sourceName = sourceNameFromFile fPath
  let errorOrAlerts = parseAlertContents sourceName contents
  case errorOrAlerts of
    Left parseError -> return $ Left [ParsecError parseError]
    Right errorsAndAlerts -> let (errors, alerts) = partitionEithers errorsAndAlerts in
      if not (null errors)
        then return $ Left (map (attachFileName sourceName) errors)
        else return $ Right alerts

parseAlertContents :: FilePath -> String -> Either ParseError [Either OWAParseError OWAAlert]
parseAlertContents sourceName = Text.Parsec.runParser
  (do
    commentOrSpacesParser
    results <- alertParser `endBy` commentOrSpacesParser
    eof
    return results)
  GenericParserState {
    indentationLevel = [],
    shouldUpdate = False
  } 
  sourceName

---------------------------------------------------------------------------
--------------------PARSERS------------------------------------------------
---------------------------------------------------------------------------

alertParser :: GenParser Char GenericParserState (Either OWAParseError OWAAlert)
alertParser = do
  name <- nameParserWithKeyword alertKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  attrs <- alertAttrLine `sepEndBy1` many (Text.Parsec.try indentedComment)
  modifyState reduceIndentationLevel
  let attrMap = Map.fromList attrs
  let maybeAlert = alertFromNameAndAttrMap name attrMap
  case maybeAlert of
    Nothing -> return $ Left ObjectError {
      fileName = "",
      itemName = name,
      missingRequiredAttributes = missingAttrs attrMap
    }
    Just alert -> return $ Right alert

alertAttrLine :: GenParser Char GenericParserState (AlertAttr, AlertVal)
alertAttrLine = indentParser $ choice alertAttrParsers

alertAttrParsers :: [GenParser Char GenericParserState (AlertAttr, AlertVal)]
alertAttrParsers = map (Text.Parsec.try . localizedKeyParserWithKeyword) attributeKeywords

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

missingAttrs :: AlertAttrMap -> [AlertAttr]
missingAttrs attrMap = (requiredAttributes \\ Map.keys attrMap) ++ buttonFormat
  where containsSingleButtonFormat = Map.member dismissButtonKeyword attrMap ||
                                  Map.member neutralButtonKeyword attrMap
        containsYes = Map.member yesButtonKeyword attrMap
        containsNo = Map.member noButtonKeyword attrMap
        isValid = containsSingleButtonFormat || (containsYes && containsNo)
        buttonFormat 
          | isValid = []
          | containsYes = ["NoButton"]
          | containsNo = ["YesButton"]
          | otherwise = ["Any Button Format"]

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

buttonFormatKeywordPlaceholder :: AlertAttr
buttonFormatKeywordPlaceholder = "ButtonFormat"

requiredAttributes :: [AlertAttr]
requiredAttributes = [titleKeyword, messageKeyword]

attributeKeywords = [titleKeyword,
  messageKeyword,
  dismissButtonKeyword,
  neutralButtonKeyword,
  yesButtonKeyword,
  noButtonKeyword]
