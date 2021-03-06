{-|
Module      : Parse.ColorParser
Description : Module for parsing values from a .colors file into OWAColor models
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Parse.ColorParser 
  ( parseColorsFromFile ) where

import           Data.Either
import           Data.List
import qualified Data.Map.Strict as Map
import           Text.Parsec
import           Text.ParserCombinators.Parsec

import           Model.OWAColor
import           Model.OWAParseError
import           Parse.Utils

type ColorAttr = String
type ColorVal = Float
type ColorAttrMap = Map.Map ColorAttr ColorVal

-------------------------------------------------------------------------------
----------------------ENTRY METHODS--------------------------------------------
-------------------------------------------------------------------------------

-- | 'parseColorsFromFile' takes a file, reads its contents,
-- and returns a list of colors contained in the file.
parseColorsFromFile :: FilePath -> IO (Either [OWAParseError] [OWAColor])
parseColorsFromFile fPath = do
  contents <- readFile fPath
  let source = sourceNameFromFile fPath
  let errorOrColors = parseColorContents source contents
  case errorOrColors of
    Left parseError -> return (Left [ParsecError parseError])
    Right errorsAndColors -> let (errors, colors) = partitionEithers errorsAndColors in
      if not (null errors)
        then return $ Left errors
        else return $ Right colors

-- 'parseColorContents' takes a string representing file contents,
-- and returns either a ParseError if the string could not be parsed,
-- or a list of parsed colors.
parseColorContents :: String -> String -> Either ParseError [Either OWAParseError OWAColor]
parseColorContents source = Text.Parsec.runParser
  (do
    commentOrSpacesParser
    result <- colorParser `endBy` commentOrSpacesParser
    eof
    return result)
  GenericParserState {
    indentationLevel = [],
    shouldUpdate = False,
    parseFileName = source
  }
  source

-------------------------------------------------------------------------------
-----------------------------------PARSERS-------------------------------------
-------------------------------------------------------------------------------

colorParser :: GenParser Char GenericParserState (Either OWAParseError OWAColor)
colorParser = do
  name <- nameParserWithKeyword colorKeyword
  modifyState setShouldUpdateIndentLevel
  _ <- many $ Text.Parsec.try indentedComment
  attrs <- attrLine `sepEndBy1` many (Text.Parsec.try indentedComment)
  modifyState reduceIndentationLevel
  let attrMap = Map.fromList (concat attrs)
  let maybeColor = colorFromNameAndAttrMap name attrMap
  source <- parseFileName <$> getState
  case maybeColor of
    Nothing -> return $ Left ObjectError {
      fileName = source,
      itemName = name,
      missingRequiredAttributes = missingAttrs attrMap
    }
    Just color -> return $ Right color

attrLine :: GenParser Char GenericParserState [(ColorAttr, ColorVal)]
attrLine = indentParser $ choice attrParsers

attrParsers :: [GenParser Char GenericParserState [(ColorAttr, ColorVal)]]
attrParsers = map Text.Parsec.try
  [redParser, 
  greenParser,
  blueParser, 
  alphaParser,
  hexParser]

redParser :: GenParser Char GenericParserState [(ColorAttr, ColorVal)]
redParser = do
  tuple <- floatAttributeParser redKeyword
  return [tuple]

greenParser :: GenParser Char GenericParserState [(ColorAttr, ColorVal)]
greenParser = do
  tuple <- floatAttributeParser greenKeyword
  return [tuple]

blueParser :: GenParser Char GenericParserState [(ColorAttr, ColorVal)]
blueParser = do
  tuple <- floatAttributeParser bluekeyword
  return [tuple]

alphaParser :: GenParser Char GenericParserState [(ColorAttr, ColorVal)]
alphaParser = do
  tuple <- floatAttributeParser alphaKeyword
  return [tuple]

hexParser :: GenParser Char GenericParserState [(ColorAttr, ColorVal)]
hexParser = do
  _ <- string hexKeyword
  _ <- spaceTabs
  _ <- string "0x"
  hexString <- count 6 hexChar
  maybeExtraChars <- optionMaybe (count 2 hexChar)
  singleTrailingComment
  case maybeExtraChars of
    Nothing -> return $ attrsFromHexString hexString
    Just extraChars -> return $ attrsFromHexString (hexString ++ extraChars)

hexChar :: GenParser Char GenericParserState Char
hexChar = oneOf "0123456789aAbBcCdDeEfF"

-------------------------------------------------------------------------------
--------------CONSTRUCTING ATTRIBUTES AND COLOR--------------------------------
-------------------------------------------------------------------------------

colorFromNameAndAttrMap :: String -> ColorAttrMap -> Maybe OWAColor
colorFromNameAndAttrMap name attrMap = do
  redVal <- Map.lookup redKeyword attrMap
  greenVal <-  Map.lookup greenKeyword attrMap 
  blueVal <-  Map.lookup bluekeyword attrMap 
  alphaVal <- case Map.lookup alphaKeyword attrMap of
    Just a -> Just a
    Nothing -> Just 1.0
  return $ colorFromTuple (name, redVal, greenVal, blueVal, alphaVal)

missingAttrs :: ColorAttrMap -> [ColorAttr]
missingAttrs attrMap = requiredAttributes \\ Map.keys attrMap

attrsFromHexString :: String -> [(ColorAttr, ColorVal)]
attrsFromHexString [r1,r2,g1,g2,b1,b2] = [(redKeyword, hexValFromChars r1 r2),
                                             (greenKeyword, hexValFromChars g1 g2),
                                             (bluekeyword, hexValFromChars b1 b2)]
attrsFromHexString [a1,a2,r1,r2,g1,g2,b1,b2] = [(redKeyword, hexValFromChars r1 r2),
                                             (greenKeyword, hexValFromChars g1 g2),
                                             (bluekeyword, hexValFromChars b1 b2),
                                             (alphaKeyword, hexValFromChars a1 a2 / 255.0)]
attrsFromHexString _ = []

hexValFromChars :: Char -> Char -> ColorVal
hexValFromChars c1 c2 = 16.0 * hexValFromChar c1 + hexValFromChar c2

hexValFromChar :: Char -> Float
hexValFromChar c = case c of
  '0' -> 0.0
  '1' -> 1.0
  '2' -> 2.0
  '3' -> 3.0
  '4' -> 4.0
  '5' -> 5.0
  '6' -> 6.0
  '7' -> 7.0
  '8' -> 8.0
  '9' -> 9.0
  'a' -> 10.0
  'A' -> 10.0
  'b' -> 11.0
  'B' -> 11.0
  'c' -> 12.0
  'C' -> 12.0
  'd' -> 13.0
  'D' -> 13.0
  'e' -> 14.0
  'E' -> 14.0
  'f' -> 15.0
  'F' -> 15.0
  _ -> error "Invalid Hex Value!" -- This should be impossible.

-------------------------------------------------------------------------------
-------------------KEYWORD CONSTANTS-------------------------------------------
-------------------------------------------------------------------------------

colorKeyword :: String
colorKeyword = "Color"

redKeyword :: ColorAttr
redKeyword = "Red"

greenKeyword :: ColorAttr
greenKeyword = "Green"

bluekeyword :: ColorAttr
bluekeyword = "Blue"

alphaKeyword :: ColorAttr
alphaKeyword = "Alpha"

hexKeyword :: ColorAttr
hexKeyword = "Hex"

requiredAttributes :: [ColorAttr]
requiredAttributes = [redKeyword, greenKeyword, bluekeyword]
