module OWAParseError where

import Text.Parsec.Error

data OWAParseError = ParsecError ParseError |
  ObjectError {
    itemName :: String,
    missingRequiredAttributes :: [String]
  }
