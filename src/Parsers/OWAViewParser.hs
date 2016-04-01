{-|
Module      : OWAViewParser
Description : Module for parsing a view from a .view file into a OWAView model
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAViewParser (
  parseViewFromFile
) where

import OWAParseError
import OWAView
import ParseUtil
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import Text.Parsec
import Text.Parsec.Error
import Text.ParserCombinators.Parsec

type ViewAttr = String
data ViewVal = TypeVal String
type ViewAttrMap = Map.Map ViewAttr ViewVal

-------------------------------------------------------------------------------
----------------------ENTRY METHODS--------------------------------------------
-------------------------------------------------------------------------------

-- | 'parseViewFromFile' takes a file, reads its contents,
-- and returns a view parsed from the file, or a list of errors encountered.
parseViewFromFile :: FilePath -> IO (Either [OWAParseError] OWAView)
parseViewFromFile fPath = do
  contents <- readFile fPath
  let sourceName = sourceNameFromFile fPath
  let errorOrView = parseViewContents sourceName contents
  case errorOrView of
    Left parseError -> return (Left [ParsecError parseError])
    Right (Left objectError) -> return (Left [objectError])
    Right (Right view) -> return (Right view)

parseViewContents :: String -> String -> Either ParseError (Either OWAParseError OWAView)
parseViewContents sourceName = Text.Parsec.runParser
  (do
    commentOrSpacesParser
    result <- viewParser (head (Split.splitOn "." sourceName))
    commentOrSpacesParser
    eof
    return result)
  GenericParserState {
    indentationLevel = [],
    shouldUpdate = False
  }
  sourceName

-------------------------------------------------------------------------------
-----------------------------------PARSERS-------------------------------------
-------------------------------------------------------------------------------

viewParser :: String -> GenParser Char GenericParserState (Either OWAParseError OWAView)
viewParser fileName = do
  name <- nameParserWithKeyword viewKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  attrs <- attributeParser `sepEndBy` many (Text.Parsec.try indentedComment)
  let attrMap = Map.fromList attrs
  modifyState reduceIndentationLevel
  return $ Right (viewFromNameFileAndAttrMap name fileName attrMap)

attributeParser :: GenParser Char GenericParserState (ViewAttr, ViewVal)
attributeParser = indentParser $ choice allAttrParsers

allAttrParsers :: [GenParser Char GenericParserState (ViewAttr, ViewVal)]
allAttrParsers = [typeParser]

typeParser :: GenParser Char GenericParserState (ViewAttr, ViewVal)
typeParser = do
  (_, typeName) <- variableNameParserWithKeyword typeKeyword 
  return (typeKeyword, TypeVal typeName)

-------------------------------------------------------------------------------
-----------------------------------CONSTRUCTING VIEWS--------------------------
-------------------------------------------------------------------------------

viewFromNameFileAndAttrMap :: String -> String -> ViewAttrMap -> OWAView
viewFromNameFileAndAttrMap name fileName attrMap = OWAView {
  viewName = name,
  viewType = vType,
  subviews = [],
  constraints = []
}
  where vType = case Map.lookup typeKeyword attrMap of
          Nothing -> fileName
          Just (TypeVal t) -> t

-------------------------------------------------------------------------------
-----------------------------------VIEW KEYWORDS-------------------------------
-------------------------------------------------------------------------------

viewKeyword :: String
viewKeyword = "View"

typeKeyword :: String
typeKeyword = "Type"
