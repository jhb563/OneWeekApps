{-|
Module      : OWAParseError
Description : Module encapsulating possible parse errors in OWA.
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAParseError (
  OWAParseError(..),
  ErrorInfo,
  errorFromInfo
) where

import Data.List
import Text.Parsec.Error
import Text.Parsec.Pos

-- | OWAParseErrors can take two forms. They are either a wrapper
-- around an error returned by Parsec, or they are a description of
-- an insufficiently attributed item. 
data OWAParseError = ParsecError ParseError |
  ObjectError {
    itemName :: String,
    missingRequiredAttributes :: [String]
  } deriving (Show)

instance Eq OWAParseError where
  (==) error1 error2 = itemName error1 == itemName error2 &&
                        (sort (missingRequiredAttributes error1) ==
                        sort (missingRequiredAttributes error2))

-------------------------------------------------------------------------------
-------------------BUILDING PARSE ERRORS---------------------------------------
-------------------------------------------------------------------------------

type ErrorInfo = (SourceName, Line, Column, String)

errorFromInfo :: FilePath -> ErrorInfo -> ParseError
errorFromInfo filePath (name, line, col, msg) = newErrorMessage message srcPos
  where message = Expect msg
        srcPos = newPos (filePath ++ name) line col

