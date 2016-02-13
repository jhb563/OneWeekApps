{-|
Module      : OWAColorParser
Description : Module for parsing values from a .colors file into OWAColor models
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAColorParser (
  parseColorsFromFile,
) where

import Data.Either
import OWAColor
import System.IO
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as Map

type ColorAttr = String
type ColorVal = Float
type ColorAttrMap = Map.Map ColorAttr ColorVal

parseColorsFromFile :: FilePath -> IO ([OWAColor])
parseColorsFromFile fPath = do
  contents <- readFile fPath
  let result = parseColorContents contents
  either colorListFromError colorListFromList result

parseColorContents :: String -> Either ParseError [OWAColor]
parseColorContents contents = parse manyColorParser "" contents

manyColorParser :: GenParser Char st [OWAColor]
manyColorParser = many colorParser

colorParser :: GenParser Char st OWAColor
colorParser = do
  spaces
  string "Color"
  char ' '
  firstLetter <- lower
  rest <- many alphaNum
  let name = firstLetter:rest
  endOfLine
  attrs <- many1 attrLine
  let attrMap = Map.fromList (concat attrs)
  finalColor <- case (colorFromNameAndAttrMap name attrMap) of
    Just color -> return color
    Nothing -> return $ colorFromTuple (name, 0.0, 0.0, 0.0, 0.0)
  return finalColor

-- Try the different parser for possible attributes. 
attrLine :: GenParser Char st [(ColorAttr, ColorVal)]
attrLine = do
  string "\t" <|> string "  "
  attrs <- choice attrParsers
  return attrs

attrParsers :: [GenParser Char st [(ColorAttr, ColorVal)]]
attrParsers = [redParser, greenParser, blueParser, alphaParser, hexParser]

redParser :: GenParser Char st [(ColorAttr, ColorVal)]
redParser = do
  tuple <- singleAttrParser redKeyword
  return [tuple]

greenParser :: GenParser Char st [(ColorAttr, ColorVal)]
greenParser = do
  tuple <- singleAttrParser greenKeyword
  return [tuple]

blueParser :: GenParser Char st [(ColorAttr, ColorVal)]
blueParser = do
  tuple <- singleAttrParser bluekeyword
  return [tuple]

alphaParser :: GenParser Char st [(ColorAttr, ColorVal)]
alphaParser = do
  tuple <- singleAttrParser alphaKeyword
  return [tuple]

hexParser :: GenParser Char st [(ColorAttr, ColorVal)]
hexParser = do
  string hexKeyword
  char ' '
  string "0x"
  hexString <- many hexChar
  endOfLine
  let attrs = attrsFromHexString hexString
  return attrs

singleAttrParser :: ColorAttr -> GenParser Char st (ColorAttr, ColorVal)
singleAttrParser keyword = do
  string keyword
  char ' '
  value <- parseFloat
  endOfLine
  return (keyword, value)

hexChar :: GenParser Char st Char
hexChar = oneOf "0123456789aAbBcCdDeEfF"

attrsFromHexString :: String -> [(ColorAttr, ColorVal)]
attrsFromHexString (r1:r2:g1:g2:b1:b2:[]) = [(redKeyword, hexValFromChars r1 r2),
                                             (greenKeyword, hexValFromChars g1 g2),
                                             (bluekeyword, hexValFromChars b1 b2)]
attrsFromHexString (a1:a2:r1:r2:g1:g2:b1:b2:[]) = [(redKeyword, hexValFromChars r1 r2),
                                             (greenKeyword, hexValFromChars g1 g2),
                                             (bluekeyword, hexValFromChars b1 b2),
                                             (alphaKeyword, (hexValFromChars a1 a2) / 255.0)]
attrsFromHexString invalidHexString = []

hexValFromChars :: Char -> Char -> ColorVal
hexValFromChars c1 c2 = (16.0 * (hexValFromChar c1)) + (hexValFromChar c2)

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

-- Try to get some digits. If there are no digits, we MUST have . followed by at least one digit
-- If there are initially digits, then the . and following digits are optional.
parseFloat :: GenParser Char st ColorVal
parseFloat = do
  wholeNumberString <- many digit
  let wholeNumberPortion = if length wholeNumberString > 0
                            then read wholeNumberString :: Float
                            else 0.0
  decimalPortion <- if length wholeNumberString > 0 
    then option 0.0 decimalAndFollowing 
    else decimalAndFollowing
  return $ wholeNumberPortion + decimalPortion

decimalAndFollowing :: GenParser Char st ColorVal
decimalAndFollowing = do
  char '.'
  following <- many digit
  let asFloat = if length following > 0  
                  then read ('0':'.':following) :: Float 
                  else 0.0
  return asFloat

colorListFromError :: ParseError -> IO [OWAColor]
colorListFromError e = do
  putStrLn "ERROR PARSING"
  mapM (putStrLn . showMessage) (errorMessages e)
  let src = errorPos e
  putStrLn $ sourceName src
  putStrLn (show $ sourceLine src)
  putStrLn (show $ sourceColumn src)
  putStrLn "END ERROR"
  return []

colorListFromList :: [OWAColor] -> IO [OWAColor]
colorListFromList l = return l

showMessage :: Message -> String
showMessage (SysUnExpect str) = "SysUnExpect " ++ str
showMessage (UnExpect str) = "UnExpect " ++ str
showMessage (Expect str) = "Expect " ++ str
showMessage (Message str) = "Message " ++ str

colorFromNameAndAttrMap :: String -> ColorAttrMap -> Maybe OWAColor
colorFromNameAndAttrMap name attrMap = do
  --red <- return $ fromIntegral $ length $ Map.keys attrMap -- Map.lookup redKeyword attrMap
  red <- Map.lookup redKeyword attrMap
  green <-  Map.lookup greenKeyword attrMap 
  blue <-  Map.lookup bluekeyword attrMap 
  alpha <- case (Map.lookup alphaKeyword attrMap) of
    Just a -> Just a
    Nothing -> Just 1.0
  return $ colorFromTuple (name, red, green, blue, alpha)

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

colorKeywords :: [ColorAttr]
colorKeywords = [redKeyword, greenKeyword, bluekeyword,
                 alphaKeyword, hexKeyword]
