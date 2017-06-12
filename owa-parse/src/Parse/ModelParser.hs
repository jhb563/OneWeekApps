{-|
Module      : Parse.ModelParser
Description : Module for parsing values from a .model file into OWAModel models
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Parse.ModelParser
  ( parseModelFromFile ) where

import           Control.Applicative ((<|>))
import           Data.List (sort)
import qualified Data.List.Split as Split
import           Data.Maybe (fromMaybe, isJust)
import           Text.Parsec (many, sepEndBy, choice, string, upper, alphaNum, char)
import qualified Text.Parsec as P
import           Text.ParserCombinators.Parsec (GenParser)

import           Model.OWAModel
import           Model.OWAParseError
import           Parse.Utils (GenericParserState(..))
import qualified Parse.Utils as U

type ModelAttr = String
data ModelVal = 
  TypeVal String |
  FieldVal OWAModelField |
  FieldErrs [OWAParseError]

type FieldAttr = String
data FieldVal = 
  ReadOnlyVal |
  FieldTypeVal OWAModelFieldType

-------------------------------------------------------------------------------
----------------------ENTRY METHODS--------------------------------------------
-------------------------------------------------------------------------------

-- | 'parseModelFromFile' takes a file, reads its contents, and returns the
-- model represented by the file, or a list of possible errors.
parseModelFromFile :: FilePath -> IO (Either [OWAParseError] OWAModel)
parseModelFromFile fPath = do
  contents <- readFile fPath
  let source = U.sourceNameFromFile fPath
  let errorOrModel = parseModelContents source contents
  case errorOrModel of
    Left parseError -> return (Left [ParsecError parseError])
    Right itemErrsOrModel -> case itemErrsOrModel of
      Left itemErrs -> return $ Left itemErrs
      Right model -> return $ Right model

parseModelContents :: String -> String -> Either P.ParseError (Either [OWAParseError] OWAModel)
parseModelContents source = P.runParser
  (do
    U.commentOrSpacesParser
    result <- modelParser (head (Split.splitOn "." source))
    U.commentOrSpacesParser
    P.eof
    return result)
  GenericParserState
    { indentationLevel = []
    , shouldUpdate = False 
    , parseFileName = source
    }
  source

-------------------------------------------------------------------------------
-----------------------------------PARSERS-------------------------------------
-------------------------------------------------------------------------------

modelParser :: String -> GenParser Char GenericParserState (Either [OWAParseError] OWAModel)
modelParser filename = do
  U.loneStringKeywordParser modelKeyword
  P.modifyState U.setShouldUpdateIndentLevel
  _ <- many $ P.try U.indentedComment
  allAttrs <- attributeParser `sepEndBy` many (P.try U.indentedComment)
  P.modifyState U.reduceIndentationLevel
  return $ modelElementsFromAttrs filename allAttrs

attributeParser :: GenParser Char GenericParserState (ModelAttr, ModelVal)
attributeParser = U.indentParser $ choice [modelTypeParser, fieldParser]

modelTypeParser :: GenParser Char GenericParserState (ModelAttr, ModelVal)
modelTypeParser = do
  (_, typeName) <- U.variableNameParserWithKeyword typeKeyword
  return (typeKeyword, TypeVal typeName)

fieldParser :: GenParser Char GenericParserState (ModelAttr, ModelVal)
fieldParser = do
  fieldName' <- U.nameParserWithKeyword fieldKeyword
  P.modifyState U.setShouldUpdateIndentLevel
  _ <- many $ P.try U.indentedComment
  fieldAttrs <- fieldAttributeParser `sepEndBy` many (P.try U.indentedComment)
  let maybeField = fieldFromAttrs fieldName' fieldAttrs
  source <- parseFileName <$> P.getState
  let finalFieldVal = case maybeField of
                        Nothing -> FieldErrs [errorForField fieldName' source]
                        Just f -> FieldVal f
  P.modifyState U.reduceIndentationLevel
  return (fieldName', finalFieldVal)
  where
    -- Currently the only field we can be missing is type
    errorForField name sourceFile = ObjectError
      { fileName = sourceFile
      , itemName = name
      , missingRequiredAttributes = ["Type"] }

fieldAttributeParser :: GenParser Char GenericParserState (FieldAttr, FieldVal)
fieldAttributeParser = U.indentParser $ choice [fieldTypeParser, readOnlyParser]

fieldTypeParser :: GenParser Char GenericParserState (FieldAttr, FieldVal)
fieldTypeParser = do
  _ <- string typeKeyword
  _ <- U.spaceTabs
  typ <- innerFieldTypeParser
  U.singleTrailingComment
  return (typeKeyword, FieldTypeVal typ)

innerFieldTypeParser :: GenParser Char GenericParserState OWAModelFieldType
innerFieldTypeParser = choice $ P.try <$>
  [ string intTypeKeyword >> return IntField
  , string floatTypeKeyword >> return FloatField
  , string boolTypeKeyword >> return BoolField
  , string stringTypeKeyword >> return StringField
  , OptionalType <$> (string maybeKeyword >> U.spaceTabs >> innerFieldTypeParser)
  , ArrayType <$> (string arrayKeyword >> U.spaceTabs >> innerFieldTypeParser)
  , MapType <$> (string mapKeyword >> U.spaceTabs >> innerFieldTypeParser)
  , CustomField <$> customNameParser ]

customNameParser :: GenParser Char GenericParserState String
customNameParser = do
  _ <- string customKeyword
  _ <- U.spaceTabs
  firstLetter <- upper
  restOfName <- many (alphaNum <|> char '_')
  let fullName = firstLetter : restOfName
  if fullName `elem` typeKeywords
    then P.unexpected "You can't use a reserved type keyword as a custom type!"
    else return fullName

readOnlyParser :: GenParser Char GenericParserState (FieldAttr, FieldVal)
readOnlyParser = do
  _ <- U.loneStringKeywordParser readOnlyKeyword
  return (readOnlyKeyword, ReadOnlyVal)

-------------------------------------------------------------------------------
-----------------------------------BUILDING OBJECTS----------------------------
-------------------------------------------------------------------------------

fieldFromAttrs :: String -> [(FieldAttr, FieldVal)] -> Maybe OWAModelField
fieldFromAttrs name attrs = do
  typeVal <- lookup typeKeyword attrs
  actualType <- case typeVal of
    FieldTypeVal typ -> Just typ
    _ -> Nothing
  let readonlyVal = lookup readOnlyKeyword attrs
  return OWAModelField 
    { fieldName = name
    , fieldType = actualType
    , fieldReadOnly = isJust readonlyVal }

modelElementsFromAttrs :: String -> [(ModelAttr, ModelVal)] -> Either [OWAParseError] OWAModel
modelElementsFromAttrs filename attrs = case errs of
  [] -> Right $ OWAModel finalType (sort fields)
  errs' -> Left errs'
  where
    (maybeType, errs, fields) = splitPieces attrs
    finalType = fromMaybe filename maybeType

splitPieces :: [(ModelAttr, ModelVal)] -> (Maybe String, [OWAParseError], [OWAModelField])
splitPieces = splitPiecesTail (Nothing, [], [])
  where
    splitPiecesTail accum [] = accum
    splitPiecesTail (prevType, prevErrs, prevFields) (nextVal : restVals) = case nextVal of
      (_, TypeVal newType) -> splitPiecesTail (Just newType, prevErrs, prevFields) restVals
      (_, FieldVal newField) -> splitPiecesTail (prevType, prevErrs, newField : prevFields) restVals
      (_, FieldErrs newErrs) -> splitPiecesTail (prevType, prevErrs ++ newErrs, prevFields) restVals

-------------------------------------------------------------------------------
-----------------------------------VIEW KEYWORDS-------------------------------
-------------------------------------------------------------------------------

modelKeyword :: String
modelKeyword = "Model"

typeKeyword :: String
typeKeyword = "Type"

fieldKeyword :: String
fieldKeyword = "Field"

readOnlyKeyword :: String
readOnlyKeyword = "ReadOnly"

intTypeKeyword :: String
intTypeKeyword = "Int"

floatTypeKeyword :: String
floatTypeKeyword = "Float"

boolTypeKeyword :: String
boolTypeKeyword = "Bool"

stringTypeKeyword :: String
stringTypeKeyword = "String"

maybeKeyword :: String
maybeKeyword = "Maybe"

arrayKeyword :: String
arrayKeyword = "Array"

mapKeyword :: String
mapKeyword = "Map"

customKeyword :: String
customKeyword = "CustomType"

typeKeywords :: [String]
typeKeywords =
  [ intTypeKeyword
  , floatTypeKeyword
  , boolTypeKeyword
  , stringTypeKeyword
  , maybeKeyword
  , arrayKeyword
  , mapKeyword ]
