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
type ErrorVal = String
type ErrorAttrMap = Map.Map String String

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
parseErrorContents = Text.Parsec.runParser (multiErrorParser `sepBy` domainParser) ("", "") ""

---------------------------------------------------------------------------
--------------------PARSERS------------------------------------------------
---------------------------------------------------------------------------

multiErrorParser :: GenParser Char (String, String) [Maybe OWAError]
multiErrorParser = errorParser `endBy` spaces

errorParser :: GenParser Char (String, String) (Maybe OWAError)
errorParser = do
  spaces
  name <- nameParserWithKeyword errorKeyword
  attrs <- many1 errorAttrLine
  let attrMap = Map.fromList attrs
  (domainName, prefix) <- getState
  return (errorFromNameAndAttrMap name attrMap domainName)

errorAttrLine :: GenParser Char (String, String) (ErrorAttr, ErrorVal)
errorAttrLine = do
  string "\t" <|> string "  "
  choice $ map Text.Parsec.try errorAttrParsers

errorAttrParsers :: [GenParser Char (String, String) (ErrorAttr, ErrorVal)]
errorAttrParsers = [variableNameParserWithKeyword domainKeyword,
  codeParser,
  localizedKeyParserWithKeyword descriptionKeyword]

domainParser :: GenParser Char (String, String) (String, Maybe String)
domainParser = do
  (_,name) <- variableNameParserWithKeyword defaultDomainKeyword
  modifyState (\_ -> (name, ""))
  return (name, Nothing)

codeParser :: GenParser Char (String, String) (ErrorAttr, ErrorVal)
codeParser = do
  string codeKeyword
  char ' '
  maybeDots <- optionMaybe (string "...")
  firstLetter <- letter
  rest <- many (alphaNum <|> char '_')
  endOfLine
  case maybeDots of
    Just dots -> return (codeKeyword, dots ++ (firstLetter:rest))
    _ -> return (codeKeyword, (firstLetter:rest))

---------------------------------------------------------------------------
--------------------CONSTRUCTING ERRORS------------------------------------
---------------------------------------------------------------------------

errorFromNameAndAttrMap :: String -> ErrorAttrMap -> String -> Maybe OWAError
errorFromNameAndAttrMap name attrMap defaultDomain = do 
  domain <- case Map.lookup domainKeyword attrMap of
    Just domain -> Just domain
    Nothing -> if null defaultDomain then Nothing else Just defaultDomain
  code <- case Map.lookup codeKeyword attrMap of
    Just ('.':'.':'.':rest) -> Just (domain ++ rest)
    Just name -> Just name
    Nothing -> Nothing
  description <- Map.lookup descriptionKeyword attrMap
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
