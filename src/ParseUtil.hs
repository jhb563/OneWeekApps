-- Util file for parsing

module ParseUtil (
  nameParserWithKeyword,
  floatAttributeParser,
  printErrorAndReturnEmpty
) where

nameParserWithKeyword :: String -> GenParser Char st String

floatAttributeParser :: String -> GenParser Char st (String, Float)

printErrorAndReturnEmpty :: ParseError -> IO [a]

