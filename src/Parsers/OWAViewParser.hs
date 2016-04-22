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

import Data.Either
import OWAElements
import OWAParseError
import OWAView
import ParseUtil
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import Text.Parsec
import Text.Parsec.Error
import Text.ParserCombinators.Parsec

type ViewAttr = String
data ViewVal = TypeVal String |
  ElementsVal [OWAViewElement] |
  ElementsErrs [OWAParseError]
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
    Right errsOrView -> return errsOrView

parseViewContents :: String -> String -> Either ParseError (Either [OWAParseError] OWAView)
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

viewParser :: String -> GenParser Char GenericParserState (Either [OWAParseError] OWAView)
viewParser fileName = do
  name <- nameParserWithKeyword viewKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  attrs <- attributeParser `sepEndBy` many (Text.Parsec.try indentedComment)
  let attrMap = Map.fromList attrs
  modifyState reduceIndentationLevel
  case Map.lookup elementErrorsKeyword attrMap of
    Nothing -> return $ Right (viewFromNameFileAndAttrMap name fileName attrMap)
    Just (ElementsErrs es) -> return $ Left es

attributeParser :: GenParser Char GenericParserState (ViewAttr, ViewVal)
attributeParser = indentParser $ choice allAttrParsers

allAttrParsers :: [GenParser Char GenericParserState (ViewAttr, ViewVal)]
allAttrParsers = [typeParser, elementsParser]

typeParser :: GenParser Char GenericParserState (ViewAttr, ViewVal)
typeParser = do
  (_, typeName) <- variableNameParserWithKeyword typeKeyword 
  return (typeKeyword, TypeVal typeName)

elementsParser :: GenParser Char GenericParserState (ViewAttr, ViewVal)
elementsParser = do
  loneStringKeywordParser elementsKeyword 
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  elements <- elementParser `sepEndBy` many (Text.Parsec.try indentedComment)
  modifyState reduceIndentationLevel
  let (errors, elems) = partitionEithers elements
  if null errors
    then return (elementsKeyword, ElementsVal elems)
    else return (elementErrorsKeyword, ElementsErrs errors)

elementParser :: GenParser Char GenericParserState (Either OWAParseError OWAViewElement)
elementParser = indentParser $ choice allElementParsers

allElementParsers :: [GenParser Char GenericParserState (Either OWAParseError OWAViewElement)]
allElementParsers = [labelElementParser, buttonElementParser, 
  textFieldElementParser, imageViewParser]

labelElementParser :: GenParser Char GenericParserState (Either OWAParseError OWAViewElement)
labelElementParser = do
  name <- nameParserWithKeyword labelKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  labelAttrs <- Text.Parsec.try labelAttributeParser `sepEndBy` many (Text.Parsec.try indentedComment)
  let attrMap = Map.fromList labelAttrs
  let maybeLabel = labelFromNameAndAttrs name attrMap
  modifyState reduceIndentationLevel
  case maybeLabel of
    Just label -> return $ Right (LabelElement label)
    Nothing -> return $ Left ObjectError {
      fileName = "",
      itemName = name,
      -- Text is the only required keyword for Labels
      missingRequiredAttributes = [textKeyword]
    }

labelAttributeParser :: GenParser Char GenericParserState (String, String)
labelAttributeParser = indentParser $ choice allLabelAttributeParsers

allLabelAttributeParsers :: [GenParser Char GenericParserState (String, String)]
allLabelAttributeParsers = map Text.Parsec.try 
  [textParser, 
  textColorParser, 
  fontParser, 
  backgroundColorParser,
  constraintsParser]

buttonElementParser :: GenParser Char GenericParserState (Either OWAParseError OWAViewElement)
buttonElementParser = do
  name <- nameParserWithKeyword buttonKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  buttonAttrs <- Text.Parsec.try buttonAttributeParser `sepEndBy` many (Text.Parsec.try indentedComment)
  let attrMap = Map.fromList buttonAttrs
  let maybeButton = buttonFromNameAndAttrs name attrMap
  modifyState reduceIndentationLevel
  case maybeButton of
    Just button -> return $ Right (ButtonElement button)
    Nothing -> return $ Left ObjectError {
      fileName = "",
      itemName = name,
      -- Text is the only required keyword for buttons 
      missingRequiredAttributes = [textKeyword]
    }

buttonAttributeParser :: GenParser Char GenericParserState (String, String)
buttonAttributeParser = indentParser $ choice allButtonAttributeParsers

allButtonAttributeParsers :: [GenParser Char GenericParserState (String, String)]
allButtonAttributeParsers = map Text.Parsec.try 
  [textParser, 
  textColorParser, 
  fontParser, 
  backgroundColorParser,
  constraintsParser]

textFieldElementParser :: GenParser Char GenericParserState (Either OWAParseError OWAViewElement)
textFieldElementParser = do
  name <- nameParserWithKeyword textFieldKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  textfieldAttrs <- Text.Parsec.try textfieldAttributeParser `sepEndBy` many (Text.Parsec.try indentedComment)
  let attrMap = Map.fromList textfieldAttrs
  let textfield = textFieldFromNameAndAttrs name attrMap
  modifyState reduceIndentationLevel
  return $ Right (TextFieldElement textfield)

textfieldAttributeParser :: GenParser Char GenericParserState (String, String)
textfieldAttributeParser = indentParser $ choice allTextfieldAttributeParsers

allTextfieldAttributeParsers :: [GenParser Char GenericParserState (String, String)]
allTextfieldAttributeParsers = map Text.Parsec.try 
  [textParser, 
  textColorParser, 
  fontParser,
  placeholderTextParser, 
  placeholderTextColorParser, 
  placeholderFontParser,
  backgroundColorParser,
  constraintsParser]

imageViewParser :: GenParser Char GenericParserState (Either OWAParseError OWAViewElement)
imageViewParser = do
  name <- nameParserWithKeyword imageViewKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  imageViewAttrs <- Text.Parsec.try imageViewAttributeParser `sepEndBy` many (Text.Parsec.try indentedComment)
  let attrMap = Map.fromList imageViewAttrs
  let maybeImageView = imageViewFromNameAndAttrs name attrMap
  many $ Text.Parsec.try indentedComment
  modifyState reduceIndentationLevel
  case maybeImageView of
    Just imageView -> return $ Right (ImageElement imageView)
    Nothing -> return $ Left ObjectError {
      fileName = "",
      itemName = name,
      -- Source is the only required image keyword
      missingRequiredAttributes = [imageSourceKeyword]
    }

imageViewAttributeParser :: GenParser Char GenericParserState (String, String)
imageViewAttributeParser = indentParser $ choice allImageViewAttributeParsers

allImageViewAttributeParsers :: [GenParser Char GenericParserState (String, String)]
allImageViewAttributeParsers = map Text.Parsec.try
  [localizedKeyParserWithKeyword imageSourceKeyword,
  constraintsParser]

textParser :: GenParser Char GenericParserState (String, String)
textParser = localizedKeyParserWithKeyword textKeyword

textColorParser :: GenParser Char GenericParserState (String, String)
textColorParser = do 
  name <- nameParserWithKeyword textColorKeyword
  return (textColorKeyword, name)

fontParser :: GenParser Char GenericParserState (String, String)
fontParser = do 
  name <- nameParserWithKeyword fontKeyword
  return (fontKeyword, name)

placeholderTextParser :: GenParser Char GenericParserState (String, String)
placeholderTextParser = localizedKeyParserWithKeyword placeholderTextKeyword

placeholderTextColorParser :: GenParser Char GenericParserState (String, String)
placeholderTextColorParser = do 
  name <- nameParserWithKeyword placeholderTextColorKeyword
  return (placeholderTextColorKeyword, name)

placeholderFontParser :: GenParser Char GenericParserState (String, String)
placeholderFontParser = do 
  name <- nameParserWithKeyword placeholderFontKeyword
  return (placeholderFontKeyword, name)

backgroundColorParser :: GenParser Char GenericParserState (String, String)
backgroundColorParser = do 
  name <- nameParserWithKeyword backgroundColorKeyword
  return (backgroundColorKeyword, name)

imageSourceParser :: GenParser Char GenericParserState (String, String)
imageSourceParser = do 
  name <- nameParserWithKeyword imageSourceKeyword
  return (imageSourceKeyword, name)

constraintsParser :: GenParser Char GenericParserState (String, String)
constraintsParser = do
  loneStringKeywordParser layoutKeyword
  return (constraintsKeyword, constraintsKeyword)

-------------------------------------------------------------------------------
-----------------------------------CONSTRUCTING VIEWS--------------------------
-------------------------------------------------------------------------------

labelFromNameAndAttrs :: String -> Map.Map String String -> Maybe OWALabel
labelFromNameAndAttrs name attrMap = do
  text <- Map.lookup textKeyword attrMap
  return OWALabel {
    labelName = name,
    labelText = text,
    labelTextColorName = Map.lookup textColorKeyword attrMap,
    labelFontName = Map.lookup fontKeyword attrMap,
    labelBackgroundColorName = Map.lookup backgroundColorKeyword attrMap
  }

textFieldFromNameAndAttrs :: String -> Map.Map String String -> OWATextField
textFieldFromNameAndAttrs name attrMap = OWATextField {
  textFieldName = name,
  textFieldText = Map.lookup textKeyword attrMap,
  textFieldColorName = Map.lookup textColorKeyword attrMap,
  textFieldFontName = Map.lookup fontKeyword attrMap,
  textFieldPlaceholderText = Map.lookup placeholderTextKeyword attrMap,
  textFieldPlaceholderTextColorName = Map.lookup placeholderTextColorKeyword attrMap,
  textFieldPlaceholderFontName = Map.lookup placeholderFontKeyword attrMap,
  textFieldBackgroundColorName = Map.lookup backgroundColorKeyword attrMap
}

buttonFromNameAndAttrs :: String -> Map.Map String String -> Maybe OWAButton
buttonFromNameAndAttrs name attrMap = do
  text <- Map.lookup textKeyword attrMap
  return OWAButton {
    buttonName = name,
    buttonText = text,
    buttonTextColorName = Map.lookup textColorKeyword attrMap,
    buttonFontName = Map.lookup fontKeyword attrMap,
    buttonBackgroundColorName = Map.lookup backgroundColorKeyword attrMap
  }

imageViewFromNameAndAttrs :: String -> Map.Map String String -> Maybe OWAImageView
imageViewFromNameAndAttrs name attrMap = do
  sourceName <- Map.lookup imageSourceKeyword attrMap
  return OWAImageView {
    imageViewName = name,
    imageSourceName = sourceName
  } 

viewFromNameFileAndAttrMap :: String -> String -> ViewAttrMap -> OWAView
viewFromNameFileAndAttrMap name fileName attrMap = OWAView {
  viewName = name,
  viewType = vType,
  subviews = elems,
  constraints = []
}
  where vType = case Map.lookup typeKeyword attrMap of
          Nothing -> fileName
          Just (TypeVal t) -> t
        elems = case Map.lookup elementsKeyword attrMap of
          Nothing -> []
          Just (ElementsVal viewElems) -> viewElems

-------------------------------------------------------------------------------
-----------------------------------VIEW KEYWORDS-------------------------------
-------------------------------------------------------------------------------

viewKeyword :: String
viewKeyword = "View"

typeKeyword :: String
typeKeyword = "Type"

elementsKeyword :: String
elementsKeyword = "Elements"

elementErrorsKeyword :: String
elementErrorsKeyword = "ElementErrors"

labelKeyword :: String
labelKeyword = "Label"

buttonKeyword :: String
buttonKeyword = "Button"

textFieldKeyword :: String
textFieldKeyword = "TextField"

imageViewKeyword :: String
imageViewKeyword = "Image"

textKeyword :: String
textKeyword = "Text"

textColorKeyword :: String
textColorKeyword = "TextColor"

fontKeyword :: String
fontKeyword = "Font"

placeholderTextKeyword :: String
placeholderTextKeyword = "PlaceholderText"

placeholderTextColorKeyword :: String
placeholderTextColorKeyword = "PlaceholderTextColor"

placeholderFontKeyword :: String
placeholderFontKeyword = "PlaceholderFont"

backgroundColorKeyword :: String
backgroundColorKeyword = "BackgroundColor"

imageSourceKeyword :: String
imageSourceKeyword = "ImageSrc"

constraintsKeyword :: String
constraintsKeyword = "Constraints"

layoutKeyword :: String
layoutKeyword = "Layout"
