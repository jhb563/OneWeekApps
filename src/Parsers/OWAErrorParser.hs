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

import Control.Monad.State.Lazy
import Data.Maybe
import OWAError
import ParseUtil
import qualified Data.Map.Strict as Map
import Text.Parsec
import Text.Parsec.Error
import Text.ParserCombinators.Parsec

type ErrorAttr = String
data ErrorVal = NormalValue String |
  PrefixedValue String
type ErrorAttrMap = Map.Map ErrorAttr ErrorVal 

---------------------------------------------------------------------------
--------------------ENTRY METHODS------------------------------------------
---------------------------------------------------------------------------

-- | 'parseErrorsFromFile' takes a file, reads its contents, and returns
-- a list of errors contained in the file.
parseErrorsFromFile :: FilePath -> IO [OWAError]
parseErrorsFromFile fPath = do
  contents <- readFile fPath
  let errorOrOWAErrors = parseErrorContents contents
  either printErrorAndReturnEmpty (return . catMaybes . concat) errorOrOWAErrors

parseErrorContents :: String -> Either ParseError [[Maybe OWAError]]
parseErrorContents = Text.Parsec.runParser 
  (multiErrorParser `sepEndBy` defaultDomainParser) 
  ErrorParserState {
    currentDomain = "",
    currentPrefix = Nothing,
    errorIndentationLevel = [],
    errorShouldUpdateIndent = False
  }
  ""

---------------------------------------------------------------------------
--------------------ERROR STATE--------------------------------------------
---------------------------------------------------------------------------

data ErrorParserState = ErrorParserState {
  currentDomain :: String,
  currentPrefix :: Maybe String,
  errorIndentationLevel :: [String],
  errorShouldUpdateIndent :: Bool
}

instance ParserState ErrorParserState where
  currentIndentLevel = errorIndentationLevel
  shouldUpdateIndentLevel = errorShouldUpdateIndent
  addIndentationLevel newLevel currentState = currentState {
    errorIndentationLevel = errorIndentationLevel currentState ++ [newLevel],
    errorShouldUpdateIndent = False
  }
  reduceIndentationLevel currentState = currentState {
    errorIndentationLevel = init $ errorIndentationLevel currentState,
    errorShouldUpdateIndent = False
  }
  setShouldUpdateIndentLevel currentState = currentState {
    errorIndentationLevel = errorIndentationLevel currentState,
    errorShouldUpdateIndent = True
  }

updateDefaultDomain :: String -> Maybe String -> ErrorParserState -> ErrorParserState
updateDefaultDomain defaultDomain Nothing currentState = ErrorParserState {
  currentDomain = defaultDomain,
  currentPrefix = Nothing,
  errorIndentationLevel = errorIndentationLevel currentState,
  errorShouldUpdateIndent = False
}
updateDefaultDomain defaultDomain (Just prefix) currentState = ErrorParserState {
  currentDomain = defaultDomain,
  currentPrefix = Just prefix,
  errorIndentationLevel = init $ errorIndentationLevel currentState,
  errorShouldUpdateIndent = False
}

---------------------------------------------------------------------------
--------------------PARSERS------------------------------------------------
---------------------------------------------------------------------------

multiErrorParser :: GenParser Char ErrorParserState [Maybe OWAError]
multiErrorParser = errorParser `endBy` commentOrSpacesParser

errorParser :: GenParser Char ErrorParserState (Maybe OWAError)
errorParser = do
  commentOrSpacesParser
  name <- nameParserWithKeyword errorKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  attrs <- errorAttrLine `sepEndBy1` many (Text.Parsec.try indentedComment)
  modifyState reduceIndentationLevel
  let attrMap = Map.fromList attrs
  parserState <- getState
  return (errorFromNameAndAttrMap name attrMap parserState)

errorAttrLine :: GenParser Char ErrorParserState (ErrorAttr, ErrorVal)
errorAttrLine = indentParser (choice $ map Text.Parsec.try errorAttrParsers)

errorAttrParsers :: [GenParser Char ErrorParserState (ErrorAttr, ErrorVal)]
errorAttrParsers = [domainParser,
  codeParser,
  descriptionParser]

domainParser :: GenParser Char ErrorParserState (ErrorAttr, ErrorVal)
domainParser = do
  (_, domainName) <- variableNameParserWithKeyword domainKeyword
  return (domainKeyword, NormalValue domainName)

codeParser :: GenParser Char ErrorParserState (ErrorAttr, ErrorVal)
codeParser = do
  string codeKeyword
  char ' '
  maybeDots <- optionMaybe (string "...")
  firstLetter <- letter
  rest <- many (alphaNum <|> char '_')
  let code = firstLetter:rest
  singleTrailingComment
  case maybeDots of
    Just _ -> return (codeKeyword, PrefixedValue code)
    _ -> return (codeKeyword, NormalValue code)

descriptionParser :: GenParser Char ErrorParserState (ErrorAttr, ErrorVal)
descriptionParser = do
  (_, localKey) <- localizedKeyParserWithKeyword descriptionKeyword
  return (descriptionKeyword, NormalValue localKey)

defaultDomainParser :: GenParser Char ErrorParserState ()
defaultDomainParser = do
  (_, name) <- variableNameParserWithKeyword defaultDomainKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  maybePrefix <-  optionMaybe $ indentParser prefixParser
  spaces
  modifyState $ updateDefaultDomain name maybePrefix

prefixParser :: GenParser Char ErrorParserState String
prefixParser = do
  (_, prefix) <- variableNameParserWithKeyword prefixKeyword 
  return prefix

---------------------------------------------------------------------------
--------------------CONSTRUCTING ERRORS------------------------------------
---------------------------------------------------------------------------

errorFromNameAndAttrMap :: String -> ErrorAttrMap -> ErrorParserState -> Maybe OWAError
errorFromNameAndAttrMap name attrMap errorState = do 
  let defaultDomain = currentDomain errorState
  let maybePrefix = currentPrefix errorState
  domain <- case Map.lookup domainKeyword attrMap of
    Just (NormalValue domain) -> Just domain
    Nothing -> if null defaultDomain then Nothing else Just defaultDomain
  code <- case Map.lookup codeKeyword attrMap of
    Just (PrefixedValue code) -> case maybePrefix of
      Just prefix -> Just (prefix ++ code)
      _ -> Just (domain ++ code)
    Just (NormalValue code) -> Just code 
    Nothing -> Nothing
  description <- case Map.lookup descriptionKeyword attrMap of
    Just (NormalValue description) -> Just description
    _ -> Nothing
  return OWAError {
    errorName = name,
    errorDomain = domain,
    errorCode = code,
    errorDescription = description
  } 

---------------------------------------------------------------------------
--------------------ERROR KEYWORDS-----------------------------------------
---------------------------------------------------------------------------

errorKeyword :: String
errorKeyword = "Error"

domainKeyword :: String
domainKeyword = "Domain"

codeKeyword :: String
codeKeyword = "Code"

descriptionKeyword :: String
descriptionKeyword = "Description"

defaultDomainKeyword :: String
defaultDomainKeyword = "DefaultDomain"

prefixKeyword :: String
prefixKeyword = "Prefix"
