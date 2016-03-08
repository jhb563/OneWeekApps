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
  either printErrorAndReturnEmpty (return . catMaybes) errorOrOWAErrors

parseErrorContents :: String -> Either ParseError [Maybe OWAError]
parseErrorContents = parse (errorParser `endBy` spaces) ""

---------------------------------------------------------------------------
--------------------PARSERS------------------------------------------------
---------------------------------------------------------------------------

errorParser :: GenParser Char st (Maybe OWAError)
errorParser = do
  spaces
  name <- nameParserWithKeyword errorKeyword
  attrs <- many1 errorAttrLine
  let attrMap = Map.fromList attrs
  return (errorFromNameAndAttrMap name attrMap)

errorAttrLine :: GenParser Char st (ErrorAttr, ErrorVal)
errorAttrLine = do
  string "\t" <|> string "  "
  choice $ map Text.Parsec.try errorAttrParsers

errorAttrParsers :: [GenParser Char str (ErrorAttr, ErrorVal)]
errorAttrParsers = [variableNameParserWithKeyword domainKeyword,
  variableNameParserWithKeyword codeKeyword,
  localizedKeyParserWithKeyword descriptionKeyword]

---------------------------------------------------------------------------
--------------------CONSTRUCTING ERRORS------------------------------------
---------------------------------------------------------------------------

errorFromNameAndAttrMap :: String -> ErrorAttrMap -> Maybe OWAError
errorFromNameAndAttrMap name attrMap = do
  domain <- Map.lookup domainKeyword attrMap
  code <- Map.lookup codeKeyword attrMap
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
