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
type ErrorParserState = (String, Maybe String)

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
parseErrorContents = Text.Parsec.runParser (multiErrorParser `sepEndBy` defaultDomainParser) ("", Nothing) ""

---------------------------------------------------------------------------
--------------------PARSERS------------------------------------------------
---------------------------------------------------------------------------

multiErrorParser :: GenParser Char ErrorParserState [Maybe OWAError]
multiErrorParser = errorParser `endBy` spaces

errorParser :: GenParser Char ErrorParserState (Maybe OWAError)
errorParser = do
  spaces
  name <- nameParserWithKeyword errorKeyword
  attrs <- many1 errorAttrLine
  let attrMap = Map.fromList attrs
  parserState <- getState
  return (errorFromNameAndAttrMap name attrMap parserState)

errorAttrLine :: GenParser Char ErrorParserState (ErrorAttr, ErrorVal)
errorAttrLine = do
  string "\t" <|> string "  "
  choice $ map Text.Parsec.try errorAttrParsers

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
  endOfLine
  case maybeDots of
    Just _ -> return (codeKeyword, PrefixedValue code)
    _ -> return (codeKeyword, NormalValue code)

descriptionParser :: GenParser Char ErrorParserState (ErrorAttr, ErrorVal)
descriptionParser = do
  (_, localKey) <- localizedKeyParserWithKeyword descriptionKeyword
  return (descriptionKeyword, NormalValue localKey)

defaultDomainParser :: GenParser Char ErrorParserState (String, Maybe String)
defaultDomainParser = do
  (_,name) <- variableNameParserWithKeyword defaultDomainKeyword
  maybePrefix <- optionMaybe prefixParser
  spaces
  putState (name, maybePrefix)
  return (name, Nothing)

prefixParser :: GenParser Char ErrorParserState String
prefixParser = do
  string "\t" <|> string "  "
  (_,prefix) <- variableNameParserWithKeyword prefixKeyword 
  return prefix

---------------------------------------------------------------------------
--------------------CONSTRUCTING ERRORS------------------------------------
---------------------------------------------------------------------------

errorFromNameAndAttrMap :: String -> ErrorAttrMap -> ErrorParserState -> Maybe OWAError
errorFromNameAndAttrMap name attrMap (defaultDomain, maybePrefix) = do 
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
