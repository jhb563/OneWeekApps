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
import qualified Data.Map.Strict as Map
import Text.Parsec.Error

type FontAttr = String
data FontVal = FamilyVal String |
  SizeVal Float |
  StyleAttrs [String]
type FontAttrMap = Map.Map FontAttr FontVal

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

-------------------------------------------------------------------------------
-----------------------------------PARSERS-------------------------------------
-------------------------------------------------------------------------------

fontParser :: GenParser Char st (Maybe OWAColor)
fontParser = do
  spaces
  name <- nameParserWithKeyword fontKeyword
  attrs <- many1 fontAttrLine
  let attrMap = Map.fromList attrs
  return (fontFromNameAndAttrMap name attrMap)

fontAttrLine :: GenParser Char st (FontAttr, FontVal)
fontAttrLine = do
  string "\t" <|> string "  "
  choice fontAttrParsers

fontAttrParsers :: [GenParser Char st (FontAttr, FontVal)]
fontAttrParsers = [fontFamilyParser, fontSizeParser, fontStylesParser]

fontFamilyParser :: GenParser Char st (FontAttr, FontVal)
fontFamilyParser = do
  string fontFamilyKeyword
  char ' '
  familyName <- many (alphaNum <|> char '-')
  return (fontFamilyKeyword, FamilyVal familyName)

fontSizeParser :: GenParser Char st (FontAttr, FontVal)
fontSizeParser = do
  (_, floatVal) <- floatAttributeParser fontSizeKeyword
  return (fontSizeKeyword, SizeVal floatVal)

fontStylesParser :: GenParser Char st (FontAttr, FontVal)
fontStylesParser = do
  string fontStylesKeyword
  char ' '
  -- TODO: Comma Separated List of style vals

-------------------------------------------------------------------------------
--------------CONSTRUCTING FONTS-----------------------------------------------
-------------------------------------------------------------------------------

fontFromNameAndAttrMap :: String -> FontAttrMap -> Maybe OWAFont

-------------------------------------------------------------------------------
-------------------FONT KEYWORDS-----------------------------------------------
-------------------------------------------------------------------------------

fontKeyword :: String
fontKeyword = "Font"

fontFamilyKeyword :: String
fontFamilyKeyword = "FontFamily"

fontSizeKeyword :: String
fontSizeKeyword = "Size"

fontStylesKeyword :: String
fontStylesKeyword = "Styles"
