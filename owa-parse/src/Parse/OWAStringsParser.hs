{-|
Module      : Parse.OWAStringsParser
Description : Module for parsing strings from a .strings file into a string map
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Parse.OWAStringsParser (
  parseStringsFromFile,
) where

import           Control.Monad.Identity
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import           Text.Parsec
import           Text.Parsec.Error
import           Text.ParserCombinators.Parsec

import           Model.OWALocalizedStringSet
import           Model.OWAParseError
import           Parse.Utils

type LocalizedStringMap = Map.Map String String

---------------------------------------------------------------------------
--------------------ENTRY METHODS------------------------------------------
---------------------------------------------------------------------------

-- | 'parseStringsFromFile' takes a file, reads its contents, and returns
-- a mapping between the string keys and the string values in it, or a
-- parse error.
parseStringsFromFile :: FilePath -> IO (Either [OWAParseError] OWALocalizedStringSet)
parseStringsFromFile fPath = do
  contents <- readFile fPath
  let sourceName = sourceNameFromFile fPath
  let errorOrLocalizedStringMap = parseStringContents sourceName contents
  case errorOrLocalizedStringMap of
    Left parseError -> return $ Left [ParsecError parseError]
    Right stringMap -> return $ Right OWALocalizedStringSet {
      setName = head (Split.splitOn "." sourceName),
      setMap = stringMap
    }

parseStringContents :: FilePath -> String -> Either ParseError LocalizedStringMap
parseStringContents = Text.Parsec.runParser
  (do
    commentOrSpacesParser
    stringTuples <- stringParser `endBy` commentOrSpacesParser
    eof
    return $ Map.fromList stringTuples)
  Identity

---------------------------------------------------------------------------
--------------------PARSERS------------------------------------------------
---------------------------------------------------------------------------

stringParser :: GenParser Char st (String, String)
stringParser = do
  oneLineSpaces
  key <- parseLocalizedKey
  oneLineSpaces
  char '='
  oneLineSpaces
  value <- parseLocalizedKey
  optionMaybe (char ';')
  return (key, value)

oneLineSpaces :: GenParser Char st String 
oneLineSpaces = many (oneOf " \t")
