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
import Text.Parsec
import Text.Parsec.Error
import Text.ParserCombinators.Parsec

type FontAttr = String
data FontVal = FamilyVal String |
  SizeVal Float |
  StyleAttrs [String]
type FontAttrMap = Map.Map FontAttr FontVal

---------------------------------------------------------------------------
--------------------ENTRY METHODS------------------------------------------
---------------------------------------------------------------------------

-- | 'parseFontsFromFile' takes a file, reads its contents,
-- and returns a list of fonts contained in the file.
parseFontsFromFile :: FilePath -> IO [OWAFont]
parseFontsFromFile fPath = do
  contents <- readFile fPath
  let errorOrFonts = parseFontContents contents
  either printErrorAndReturnEmpty (return . catMaybes) errorOrFonts

parseFontContents :: String -> Either ParseError [Maybe OWAFont]
parseFontContents = Text.Parsec.runParser
  (fontParser `endBy` commentOrSpacesParser)
  (GenericParserState {
    indentationLevel = [],
    shouldUpdate = False
  })
  ""

-------------------------------------------------------------------------------
-----------------------------------PARSERS-------------------------------------
-------------------------------------------------------------------------------

fontParser :: GenParser Char GenericParserState (Maybe OWAFont)
fontParser = do
  commentOrSpacesParser
  name <- nameParserWithKeyword fontKeyword
  modifyState setShouldUpdate
  many $ Text.Parsec.try fontIndentedComment
  attrs <- fontAttrLine `sepEndBy1` many (Text.Parsec.try fontIndentedComment)
  modifyState reduceIndentationLevel
  let attrMap = Map.fromList attrs
  return (fontFromNameAndAttrMap name attrMap)

fontIndentedComment :: GenParser Char GenericParserState ()
fontIndentedComment = indentParser singleTrailingComment

fontAttrLine :: GenParser Char GenericParserState (FontAttr, FontVal)
fontAttrLine = indentParser $ choice fontAttrParsers

fontAttrParsers :: [GenParser Char GenericParserState (FontAttr, FontVal)]
fontAttrParsers = map Text.Parsec.try [fontFamilyParser, fontSizeParser, fontStylesParser]

fontFamilyParser :: GenParser Char GenericParserState (FontAttr, FontVal)
fontFamilyParser = do
  string fontFamilyKeyword
  char ' '
  familyName <- many (alphaNum <|> char '-')
  singleTrailingComment
  return (fontFamilyKeyword, FamilyVal familyName)

fontSizeParser :: GenParser Char GenericParserState (FontAttr, FontVal)
fontSizeParser = do
  (_, floatVal) <- floatAttributeParser fontSizeKeyword
  return (fontSizeKeyword, SizeVal floatVal)

fontStylesParser :: GenParser Char GenericParserState (FontAttr, FontVal)
fontStylesParser = do
  string fontStylesKeyword
  char ' '
  attributes <- fontStyleAttributeParser `sepBy1` string ", "
  singleTrailingComment
  return (fontStylesKeyword, StyleAttrs attributes)

fontStyleAttributeParser :: GenParser Char GenericParserState String
fontStyleAttributeParser = choice styleAttributeParsers

styleAttributeParsers :: [GenParser Char GenericParserState String]
styleAttributeParsers = map  (Text.Parsec.try . string) styleAttributeStrings

-------------------------------------------------------------------------------
--------------CONSTRUCTING FONTS-----------------------------------------------
-------------------------------------------------------------------------------

fontFromNameAndAttrMap :: String -> FontAttrMap -> Maybe OWAFont
fontFromNameAndAttrMap name attrMap = do
  familyName <- case Map.lookup fontFamilyKeyword attrMap of
    Just (FamilyVal name) -> Just name
    _ -> Nothing
  size <- case Map.lookup fontSizeKeyword attrMap of
    Just (SizeVal size) -> Just size
    _ -> Nothing
  styleAttrs <- case Map.lookup fontStylesKeyword attrMap of
    Just (StyleAttrs attrs) -> Just $ map (\str -> read str :: FontStyle) attrs   
    Nothing -> Just []
    _ -> Nothing
  return OWAFont {
    fontName = name,
    fontFamily = familyName,
    fontSize = size,
    fontStyles = styleAttrs
  }

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

styleAttributeStrings :: [String]
styleAttributeStrings = ["Thin",
  "Light",
  "Regular",
  "Medium",
  "Bold",
  "Italic"]
