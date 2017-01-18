{-|
Module      : Parse.ErrorParser
Description : Module for parsing errors from a .errors file into OWAError models
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Parse.ErrorParser (
  parseErrorsFromFile
) where

import           Control.Monad.State.Lazy
import           Data.Either
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Error
import           Text.ParserCombinators.Parsec

import           Model.OWAError
import           Model.OWAParseError
import           Parse.Utils

type ErrorAttr = String
data ErrorVal = NormalValue String |
  PrefixedValue String
type ErrorAttrMap = Map.Map ErrorAttr ErrorVal 

---------------------------------------------------------------------------
--------------------ENTRY METHODS------------------------------------------
---------------------------------------------------------------------------

-- | 'parseErrorsFromFile' takes a file, reads its contents, and returns
-- a list of errors contained in the file.
parseErrorsFromFile :: FilePath -> IO (Either [OWAParseError] [OWAError])
parseErrorsFromFile fPath = do
  contents <- readFile fPath
  let sourceName = sourceNameFromFile fPath
  let errorOrOWAErrors = parseErrorContents sourceName contents
  case errorOrOWAErrors of
    Left parseError -> return $ Left [ParsecError parseError]
    Right errorsAndOWAErrors -> let (errors, owaErrors) = partitionEithers (concat errorsAndOWAErrors) in
      if not (null errors)
        then return $ Left (map (attachFileName sourceName) errors)
        else return $ Right owaErrors

parseErrorContents :: FilePath -> String -> Either ParseError [[Either OWAParseError OWAError]]
parseErrorContents = Text.Parsec.runParser 
  (do
    results <- multiErrorParser `sepEndBy` defaultDomainParser
    eof
    return results) 
  ErrorParserState {
    currentDomain = "",
    currentPrefix = Nothing,
    errorIndentationLevel = [],
    errorShouldUpdateIndent = False
  }

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
  setShouldNotUpdateLevel currentState = currentState {
    errorShouldUpdateIndent = False
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

multiErrorParser :: GenParser Char ErrorParserState [Either OWAParseError OWAError]
multiErrorParser = do
  commentOrSpacesParser
  errorParser `endBy` commentOrSpacesParser

errorParser :: GenParser Char ErrorParserState (Either OWAParseError OWAError)
errorParser = do
  name <- nameParserWithKeyword errorKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  attrs <- errorAttrLine `sepEndBy1` many (Text.Parsec.try indentedComment)
  modifyState reduceIndentationLevel
  let attrMap = Map.fromList attrs
  parserState <- getState
  let maybeError = errorFromNameAndAttrMap name attrMap parserState
  case maybeError of
    Nothing -> return $ Left ObjectError {
      fileName = "",
      itemName = name,
      missingRequiredAttributes = missingAttrs attrMap parserState
    }
    Just err -> return $ Right err

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
  spaceTabs
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

missingAttrs :: ErrorAttrMap -> ErrorParserState -> [String]
missingAttrs attrMap state = (requiredAttributes \\ Map.keys attrMap) ++ maybeDomain
  where domainAvailable = Map.member domainKeyword attrMap ||
                          not (null $ currentDomain state)
        maybeDomain = if domainAvailable then [] else [domainKeyword]

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

requiredAttributes :: [String]
requiredAttributes = [codeKeyword, descriptionKeyword]

defaultDomainKeyword :: String
defaultDomainKeyword = "DefaultDomain"

prefixKeyword :: String
prefixKeyword = "Prefix"
