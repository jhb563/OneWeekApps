{-|
Module      : OWAParseError
Description : Module encapsulating possible parse errors in OWA.
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Model.OWAParseError (
  OWAParseError(..)
) where

import Data.List
import Text.Parsec.Error

-- | OWAParseErrors can take two forms. They are either a wrapper
-- around an error returned by Parsec, or they are a description of
-- an insufficiently attributed item. 
data OWAParseError = ParsecError ParseError |
  ObjectError {
    fileName :: String,
    itemName :: String,
    missingRequiredAttributes :: [String]
  }

instance Show OWAParseError where
  show (ParsecError err) = show err
  show ObjectError {fileName = fName, itemName = name, missingRequiredAttributes = attrs} = 
    "Error: Insufficient attributes for item " ++ name ++ " in file " ++ fName ++ "\nRequires: " ++ attrString
      where attrString = case attrs of
              [] -> ""
              (a:as) -> foldl (\s newS -> s ++ (',':newS)) a as

instance Eq OWAParseError where
  (==) error1 error2 = itemName error1 == itemName error2 &&
                       fileName error1 == fileName error2 &&
                        (sort (missingRequiredAttributes error1) ==
                        sort (missingRequiredAttributes error2))
