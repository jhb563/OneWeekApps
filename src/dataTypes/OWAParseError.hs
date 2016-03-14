{-|
Module      : OWAParseError
Description : Module encapsulating possible parse errors in OWA.
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAParseError where

import Text.Parsec.Error

-- | OWAParseErrors can take two forms. They are either a wrapper
-- around an error returned by Parsec, or they are a description of
-- an insufficiently attributed item. 
data OWAParseError = ParsecError ParseError |
  ObjectError {
    itemName :: String,
    missingRequiredAttributes :: [String]
  }
