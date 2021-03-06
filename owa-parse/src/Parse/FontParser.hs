{-|
Module      : Parse.FontParser
Description : Module for parsing fonts from a .fonts file into OWAFont models
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Parse.FontParser (
  parseFontsFromFile
) where

import           Data.Either
import           Data.List
import qualified Data.Map.Strict as Map
import           Text.Parsec
import           Text.ParserCombinators.Parsec

import           Model.OWAFont
import           Model.OWAParseError
import           Parse.Utils

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
parseFontsFromFile :: FilePath -> IO (Either [OWAParseError] [OWAFont])
parseFontsFromFile fPath = do
  contents <- readFile fPath
  let source = sourceNameFromFile fPath
  let errorOrFonts = parseFontContents source contents
  case errorOrFonts of
    Left parseError -> return $ Left [ParsecError parseError]
    Right errorsAndFonts -> let (errors, fonts) = partitionEithers errorsAndFonts in
      if not (null errors)
        then return $ Left errors
        else return $ Right fonts

parseFontContents :: FilePath -> String -> Either ParseError [Either OWAParseError OWAFont]
parseFontContents source = Text.Parsec.runParser
  (do
    commentOrSpacesParser
    results <- fontParser `endBy` commentOrSpacesParser
    eof
    return results)
  GenericParserState {
    indentationLevel = [],
    shouldUpdate = False,
    parseFileName = source
  }
  source

-------------------------------------------------------------------------------
-----------------------------------PARSERS-------------------------------------
-------------------------------------------------------------------------------

fontParser :: GenParser Char GenericParserState (Either OWAParseError OWAFont)
fontParser = do
  name <- nameParserWithKeyword fontKeyword
  modifyState setShouldUpdateIndentLevel
  _ <- many $ Text.Parsec.try indentedComment
  attrs <- fontAttrLine `sepEndBy1` many (Text.Parsec.try indentedComment)
  modifyState reduceIndentationLevel
  let attrMap = Map.fromList attrs
  let maybeFont = fontFromNameAndAttrMap name attrMap
  source <- parseFileName <$> getState
  case maybeFont of
    Nothing -> return $ Left ObjectError {
      fileName = source,
      itemName = name,
      missingRequiredAttributes = missingAttrs attrMap
    }
    Just font -> return $ Right font

fontAttrLine :: GenParser Char GenericParserState (FontAttr, FontVal)
fontAttrLine = indentParser $ choice fontAttrParsers

fontAttrParsers :: [GenParser Char GenericParserState (FontAttr, FontVal)]
fontAttrParsers = map Text.Parsec.try [fontFamilyParser, fontSizeParser, fontStylesParser]

fontFamilyParser :: GenParser Char GenericParserState (FontAttr, FontVal)
fontFamilyParser = do
  _ <- string fontFamilyKeyword
  _ <- spaceTabs
  familyName <- many (alphaNum <|> char '-')
  singleTrailingComment
  return (fontFamilyKeyword, FamilyVal familyName)

fontSizeParser :: GenParser Char GenericParserState (FontAttr, FontVal)
fontSizeParser = do
  (_, floatVal) <- floatAttributeParser fontSizeKeyword
  return (fontSizeKeyword, SizeVal floatVal)

fontStylesParser :: GenParser Char GenericParserState (FontAttr, FontVal)
fontStylesParser = do
  _ <- string fontStylesKeyword
  _ <- spaceTabs
  attributes <- fontStyleAttributeParser `sepBy1` (char ',' >> spaceTabs)
  singleTrailingComment
  return (fontStylesKeyword, StyleAttrs attributes)

fontStyleAttributeParser :: GenParser Char GenericParserState String
fontStyleAttributeParser = choice styleAttributeParsers

styleAttributeParsers :: [GenParser Char GenericParserState String]
styleAttributeParsers = map (Text.Parsec.try . string) styleAttributeStrings

-------------------------------------------------------------------------------
--------------CONSTRUCTING FONTS-----------------------------------------------
-------------------------------------------------------------------------------

fontFromNameAndAttrMap :: String -> FontAttrMap -> Maybe OWAFont
fontFromNameAndAttrMap name attrMap = do
  familyName <- case Map.lookup fontFamilyKeyword attrMap of
    Just (FamilyVal fVal) -> Just fVal
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

missingAttrs :: FontAttrMap -> [FontAttr]
missingAttrs attrMap = requiredAttributes \\ Map.keys attrMap

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

requiredAttributes :: [FontAttr]
requiredAttributes = [fontFamilyKeyword, fontSizeKeyword]
