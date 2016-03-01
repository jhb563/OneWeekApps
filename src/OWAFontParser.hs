{-|
Module      : OWAFontParser
Description : Module for parsing fonts from a .fonts file into OWAFont models
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAFontParser (
  parseFontsFromFile
) where

import Data.Maybe
import OWAFont
import ParseUtil
import Text.Parsec.Error

---------------------------------------------------------------------------
--------------------ENTRY METHODS------------------------------------------
---------------------------------------------------------------------------

-- | 'parseFontsFromFile' takes a file, reads its contens,
-- and returns a list of fonts contained in the file.
parseFontsFromFile :: FilePath -> IO [OWAFont]
parseFontsFromFile fPath = do
  contents <- readFile fPath
  let errorOrFonts = parseFontContents contents
  either printErrorAndReturnEmpty (return . catMaybes) errorOrFonts

parseFontContents :: String -> Either ParseError [Maybe OWAFont]
parseFontContents contents = Right []

