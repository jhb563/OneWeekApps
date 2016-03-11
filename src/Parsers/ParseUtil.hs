{-|
Module      : ParseUtil
Description : Module for common parsers and parse functions. 
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module ParseUtil (
  nameParserWithKeyword,
  variableNameParserWithKeyword,
  localizedKeyParserWithKeyword,
  floatAttributeParser,
  commentOrSpacesParser,
  printErrorAndReturnEmpty
) where

import Text.Parsec
import Text.Parsec.Error
import Text.ParserCombinators.Parsec

-------------------------------------------------------------------------------
-------------------PARSING STRING ATTRIBUTES-----------------------------------
-------------------------------------------------------------------------------

-- | Takes a string for a keyword, and returns a parser which parses that
-- keyword, a space, and then an alphanumeric name beginning with a
-- lowercase letter.
nameParserWithKeyword :: String -> GenParser Char st String
nameParserWithKeyword keyword = do
  string keyword
  char ' '
  firstLetter <- lower
  restOfName <- many alphaNum 
  endOfLine
  return (firstLetter:restOfName)

-- | Takes a string for a keyword, and returns a parser which parses that keyword,
-- a space, and then a string which begins with a letter, and contains alpha
-- numeric characters and underscores. 
variableNameParserWithKeyword :: String -> GenParser Char st (String, String)
variableNameParserWithKeyword keyword = do
  string keyword
  char ' '
  firstLetter <- letter
  restOfName <- many (alphaNum <|> char '_')
  endOfLine
  return (keyword, firstLetter:restOfName)

-- | Takes a string for a keyword, and returns a parser which parses that keyword,
-- a space, and then a localized string key, which is a set of any non new-line
-- characters surrounded by quotes, with inner quotes escaped.
localizedKeyParserWithKeyword :: String -> GenParser Char st (String, String)
localizedKeyParserWithKeyword keyword = do
  string keyword
  char ' '
  localizedKey <- parseLocalizedKey
  endOfLine
  return (keyword, localizedKey)

parseLocalizedKey :: GenParser Char st String
parseLocalizedKey = do
  char '"'
  substrings <- many (noneOf "\"\n") `endBy` char '"'
  case substrings of
    [] -> return ""
    (s:ss) -> return $ concat (s:map ('"':) ss)

-------------------------------------------------------------------------------
-------------------PARSING FLOAT ATTRIBUTES------------------------------------
-------------------------------------------------------------------------------

-- | Takes a string for a keyword, and returns a parser which parses that
-- keyword, a space, and then a float value.
floatAttributeParser :: String -> GenParser Char st (String, Float)
floatAttributeParser keyword = do
  string keyword
  char ' '
  value <- parseFloat
  endOfLine
  return (keyword, value)

parseFloat :: GenParser Char st Float 
parseFloat = do
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
-------------------PARSING COMMENTS--------------------------------------------
-------------------------------------------------------------------------------

-- | Skips over a series of spaces and comments
commentOrSpacesParser :: GenParser Char st ()
commentOrSpacesParser = do
  spaces `sepBy` commentParser
  return ()

commentParser :: GenParser Char st ()
commentParser = do
  string "//"
  many $ noneOf "\n"
  endOfLine
  return ()

-------------------------------------------------------------------------------
-------------------DEBUGGING ERRORS--------------------------------------------
-------------------------------------------------------------------------------

-- | Takes a parse error, prints a representation of that error to the screen,
-- and returns an empty list to signal to other parts of the program that the
-- parse has not turned up any elements.
printErrorAndReturnEmpty :: ParseError -> IO [a]
printErrorAndReturnEmpty e = do
  mapM_ (putStrLn . showMessage) (errorMessages e)
  let src = errorPos e
  print $ sourceName src
  print $ sourceLine src
  print $ sourceColumn src
  return []

showMessage :: Message -> String
showMessage (SysUnExpect str) = "System Unexpected " ++ str
showMessage (UnExpect str) = "Unexpected " ++ str
showMessage (Expect str) = "Expected " ++ str
showMessage (Message str) = str

