{-|
Module      : OWAParseUtil
Description : Module for common parsers and parse functions. 
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAParseUtil (
  ParserState(..),
  GenericParserState(..),
  loneStringKeywordParser,
  nameParserWithKeyword,
  nameParser,
  variableNameParserWithKeyword,
  localizedKeyParserWithKeyword,
  parseLocalizedKey,
  floatAttributeParser,
  parseFloat,
  indentParser,
  spaceTabs,
  commentOrSpacesParser,
  singleTrailingComment,
  indentedComment,
  sourceNameFromFile,
  attachFileName
) where

import qualified Data.Text 
import           Text.Parsec
import           Text.Parsec.Error
import           Text.ParserCombinators.Parsec

import           OWAParseError

-------------------------------------------------------------------------------
-------------------PARSER STATE------------------------------------------------
-------------------------------------------------------------------------------

-- | ParserState is a class type encompassing the basic properties we need from
-- an object in order for it to be a valid parser state to use some common methods
-- in this file. It primarily encompasses updating the indentation level.
class ParserState a where
  currentIndentLevel :: a -> [String]
  shouldUpdateIndentLevel :: a -> Bool
  addIndentationLevel :: String -> a -> a
  reduceIndentationLevel :: a -> a
  setShouldUpdateIndentLevel :: a -> a
  setShouldNotUpdateLevel :: a -> a

-- | GenericParserState is a basic object used by most of our parsers which
-- encompasses only those functions which related to indentation.
data GenericParserState = GenericParserState {
  indentationLevel :: [String],
  shouldUpdate :: Bool
}

instance ParserState GenericParserState where
  currentIndentLevel = indentationLevel
  shouldUpdateIndentLevel = shouldUpdate
  addIndentationLevel newLevel currentState = GenericParserState {
    indentationLevel = indentationLevel currentState ++ [newLevel],
    shouldUpdate = False
  }
  reduceIndentationLevel currentState = GenericParserState {
    indentationLevel = init $ indentationLevel currentState,
    shouldUpdate = False
  }
  setShouldUpdateIndentLevel currentState = currentState { shouldUpdate = True}
  setShouldNotUpdateLevel currentState = currentState {shouldUpdate = False}

-------------------------------------------------------------------------------
-------------------PARSING STRING ATTRIBUTES-----------------------------------
-------------------------------------------------------------------------------

-- | Takes a string for a keyword, and returns a parser which parses that string
-- followed by a possible comment and newline.
loneStringKeywordParser :: String -> GenParser Char st ()
loneStringKeywordParser keyword = do
  string keyword
  singleTrailingComment

-- | Takes a string for a keyword, and returns a parser which parses that
-- keyword, a space, and then an alphanumeric name beginning with a
-- lowercase letter.
nameParserWithKeyword :: String -> GenParser Char st String
nameParserWithKeyword keyword = do
  string keyword 
  spaceTabs
  name <- nameParser
  singleTrailingComment
  return name

-- | Parses a possible element name, consisting of a lowercase letter followed
-- by alphanumeric characters
nameParser :: GenParser Char st String
nameParser = do
  firstLetter <- lower
  restOfName <- many alphaNum
  return (firstLetter:restOfName)

-- | Takes a string for a keyword, and returns a parser which parses that keyword,
-- a space, and then a string which begins with a letter, and contains alpha
-- numeric characters and underscores. 
variableNameParserWithKeyword :: String -> GenParser Char st (String, String)
variableNameParserWithKeyword keyword = do
  string keyword
  spaceTabs
  firstLetter <- letter
  restOfName <- many (alphaNum <|> char '_')
  singleTrailingComment
  return (keyword, firstLetter:restOfName)

-- | Takes a string for a keyword, and returns a parser which parses that keyword,
-- a space, and then a localized string key, which is a set of any non new-line
-- characters surrounded by quotes, with inner quotes escaped.
localizedKeyParserWithKeyword :: String -> GenParser Char st (String, String)
localizedKeyParserWithKeyword keyword = do
  string keyword
  spaceTabs
  localizedKey <- parseLocalizedKey
  singleTrailingComment
  return (keyword, localizedKey)

-- | Parses a localized keyword, which is surrounded by quotes.
parseLocalizedKey :: GenParser Char st String
parseLocalizedKey = do
  char '"'
  localizedKeyBySection

localizedKeyBySection :: GenParser Char st String
localizedKeyBySection = do
  section <- many (noneOf "\"")
  if not (null section) && last section == '\\'
    then do
      char '"'
      rest <- localizedKeyBySection
      return $ section ++ ('"':rest)
    else do
      char '"'
      return section

-------------------------------------------------------------------------------
-------------------PARSING FLOAT ATTRIBUTES------------------------------------
-------------------------------------------------------------------------------

-- | Takes a string for a keyword, and returns a parser which parses that
-- keyword, a space, and then a float value.
floatAttributeParser :: String -> GenParser Char st (String, Float)
floatAttributeParser keyword = do
  string keyword
  spaceTabs
  value <- parsePositiveFloat
  singleTrailingComment
  return (keyword, value)

-- | Parses a float, whether positive or negative.
parseFloat :: GenParser Char st Float
parseFloat = do
  maybeNegative <- optionMaybe $ char '-'
  positiveValue <- parsePositiveFloat
  case maybeNegative of
    Just '-' -> return $ -1.0 * positiveValue
    _ -> return positiveValue

parsePositiveFloat :: GenParser Char st Float 
parsePositiveFloat = do
  wholeNumberString <- many digit
  let wholeNumberPortion = if not (null wholeNumberString)
                            then read wholeNumberString :: Float
                            else 0.0
  decimalPortion <- if not (null wholeNumberString)
    then option 0.0 decimalAndFollowing 
    else decimalAndFollowing
  return $ wholeNumberPortion + decimalPortion

decimalAndFollowing :: GenParser Char st Float
decimalAndFollowing = do
  char '.'
  following <- many digit
  let asFloat = if not (null following)
                  then read ('0':'.':following) :: Float 
                  else 0.0
  return asFloat

-------------------------------------------------------------------------------
-------------------INDENTING PARSERS-------------------------------------------
-------------------------------------------------------------------------------

-- | Takes a parser, reads the current indentation level and, if the current 
-- parser state indicates that the indentation level should be updated, then
-- it reads a new level of indentation and saves that to the state. Otherwise,
-- it just runs the parser.
indentParser :: ParserState st => GenParser Char st a -> GenParser Char st a
indentParser parser = do
  parserState <- getState
  let indentLevel = currentIndentLevel parserState
  let shouldUpdate = shouldUpdateIndentLevel parserState
  Text.Parsec.try $ string (concat indentLevel)
  if shouldUpdate
    then do
      newLevel <- spaceTabs
      modifyState (addIndentationLevel newLevel)
      parser
    else parser

-- | Reads some kind of spacing composed of spaces and tab characters.
spaceTabs :: GenParser Char st String
spaceTabs = many1 (oneOf " \t")

-------------------------------------------------------------------------------
-------------------PARSING COMMENTS--------------------------------------------
-------------------------------------------------------------------------------

-- | Skips over a series of spaces and comments
commentOrSpacesParser :: GenParser Char st ()
commentOrSpacesParser = do
  spaces `sepBy` commentParser
  return ()

-- | Parses a series of spaces, and then a single comment
singleTrailingComment :: GenParser Char st ()
singleTrailingComment = do
  many (oneOf " \t")
  option () commentParser
  endOfLine
  return ()

-- | Parses an indented comment. 
indentedComment :: ParserState st => GenParser Char st ()
indentedComment = indentParser singleTrailingComment

commentParser :: GenParser Char st ()
commentParser = do
  string "//"
  many $ noneOf "\n"
  return ()

-------------------------------------------------------------------------------
-------------------DEBUGGING ERRORS--------------------------------------------
-------------------------------------------------------------------------------

-- | Takes a full file path and returns just the filename.
sourceNameFromFile :: FilePath -> String
sourceNameFromFile fullPath = Data.Text.unpack $ 
  last (Data.Text.split (== '/') (Data.Text.pack fullPath))

-- | Attaches the filename to the error if it is an ObjectError. ParsecErrors
-- already have the filename attached.
attachFileName :: String -> OWAParseError -> OWAParseError
attachFileName _ (ParsecError err) = ParsecError err
attachFileName sourceName objectError = objectError {fileName = sourceName}
