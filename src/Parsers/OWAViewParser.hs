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
import Data.Maybe
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
  ElementsVal ([OWAViewElement], [OWAConstraint]) |
  ElementsErrs [OWAParseError]
type ElementAttr = String
data ElementVal = StringVal String |
  ConstraintsVal [OWAConstraint]
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
  results <- elementParser `sepEndBy` many (Text.Parsec.try indentedComment)
  let (errors, elemTuples) = partitionEithers results
  let (elements, constraintLists) = unzip elemTuples
  modifyState reduceIndentationLevel
  if null errors
    then return (elementsKeyword, ElementsVal (elements, concat constraintLists))
    else return (elementErrorsKeyword, ElementsErrs errors)

elementParser :: GenParser Char GenericParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
elementParser = indentParser $ choice allElementParsers

allElementParsers :: [GenParser Char GenericParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))]
allElementParsers = [labelElementParser, buttonElementParser, 
  textFieldElementParser, imageViewParser]

labelElementParser :: GenParser Char GenericParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
labelElementParser = do
  name <- nameParserWithKeyword labelKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  labelAttrsAndConstraints <- Text.Parsec.try labelAttributeParser `sepEndBy` many (Text.Parsec.try indentedComment)
  let (attrMap, constraints) = splitAttrsAndConstraints labelAttrsAndConstraints
  let modifiedConstraints = modifyConstraintsWithViewName name constraints
  let maybeLabel = labelFromNameAndAttrs name attrMap
  modifyState reduceIndentationLevel
  case maybeLabel of
    Just label -> return $ Right (LabelElement label, modifiedConstraints)
    Nothing -> return $ Left ObjectError {
      fileName = "",
      itemName = name,
      -- Text is the only required keyword for Labels
      missingRequiredAttributes = [textKeyword]
    }

labelAttributeParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
labelAttributeParser = indentParser $ choice allLabelAttributeParsers

allLabelAttributeParsers :: [GenParser Char GenericParserState (ElementAttr, ElementVal)]
allLabelAttributeParsers = map Text.Parsec.try 
  [textParser, 
  textColorParser, 
  fontParser, 
  backgroundColorParser,
  constraintsParser]

buttonElementParser :: GenParser Char GenericParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
buttonElementParser = do
  name <- nameParserWithKeyword buttonKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  buttonAttrsAndConstraints <- Text.Parsec.try buttonAttributeParser `sepEndBy` many (Text.Parsec.try indentedComment)
  let (attrMap, constraints) = splitAttrsAndConstraints buttonAttrsAndConstraints
  let modifiedConstraints = modifyConstraintsWithViewName name constraints
  let maybeButton = buttonFromNameAndAttrs name attrMap
  modifyState reduceIndentationLevel
  case maybeButton of
    Just button -> return $ Right (ButtonElement button, modifiedConstraints)
    Nothing -> return $ Left ObjectError {
      fileName = "",
      itemName = name,
      -- Text is the only required keyword for buttons 
      missingRequiredAttributes = [textKeyword]
    }

buttonAttributeParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
buttonAttributeParser = indentParser $ choice allButtonAttributeParsers

allButtonAttributeParsers :: [GenParser Char GenericParserState (ElementAttr, ElementVal)]
allButtonAttributeParsers = map Text.Parsec.try 
  [textParser, 
  textColorParser, 
  fontParser, 
  backgroundColorParser,
  constraintsParser]

textFieldElementParser :: GenParser Char GenericParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
textFieldElementParser = do
  name <- nameParserWithKeyword textFieldKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  textfieldAttrsAndConstraints <- Text.Parsec.try textfieldAttributeParser `sepEndBy` many (Text.Parsec.try indentedComment)
  let (attrMap, constraints) = splitAttrsAndConstraints textfieldAttrsAndConstraints
  let modifiedConstraints = modifyConstraintsWithViewName name constraints
  let textfield = textFieldFromNameAndAttrs name attrMap
  modifyState reduceIndentationLevel
  return $ Right (TextFieldElement textfield, modifiedConstraints)

textfieldAttributeParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
textfieldAttributeParser = indentParser $ choice allTextfieldAttributeParsers

allTextfieldAttributeParsers :: [GenParser Char GenericParserState (ElementAttr, ElementVal)]
allTextfieldAttributeParsers = map Text.Parsec.try 
  [textParser, 
  textColorParser, 
  fontParser,
  placeholderTextParser, 
  placeholderTextColorParser, 
  placeholderFontParser,
  backgroundColorParser,
  constraintsParser]

imageViewParser :: GenParser Char GenericParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
imageViewParser = do
  name <- nameParserWithKeyword imageViewKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  imageViewAttrsAndConstraints <- Text.Parsec.try imageViewAttributeParser `sepEndBy` many (Text.Parsec.try indentedComment)
  let (attrMap, constraints) = splitAttrsAndConstraints imageViewAttrsAndConstraints
  let modifiedConstraints = modifyConstraintsWithViewName name constraints
  let maybeImageView = imageViewFromNameAndAttrs name attrMap
  many $ Text.Parsec.try indentedComment
  modifyState reduceIndentationLevel
  case maybeImageView of
    Just imageView -> return $ Right (ImageElement imageView, modifiedConstraints)
    Nothing -> return $ Left ObjectError {
      fileName = "",
      itemName = name,
      -- Source is the only required image keyword
      missingRequiredAttributes = [imageSourceKeyword]
    }

imageViewAttributeParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
imageViewAttributeParser = indentParser $ choice allImageViewAttributeParsers

allImageViewAttributeParsers :: [GenParser Char GenericParserState (ElementAttr, ElementVal)]
allImageViewAttributeParsers = map Text.Parsec.try
  [imageSourceParser,
  constraintsParser]

textParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
textParser = do
  (attr, val) <- localizedKeyParserWithKeyword textKeyword
  return (attr, StringVal val)
  
textColorParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
textColorParser = do 
  name <- nameParserWithKeyword textColorKeyword
  return (textColorKeyword, StringVal name)

fontParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
fontParser = do 
  name <- nameParserWithKeyword fontKeyword
  return (fontKeyword, StringVal name)

placeholderTextParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
placeholderTextParser = do
  (attr, val) <- localizedKeyParserWithKeyword placeholderTextKeyword
  return (attr, StringVal val)

placeholderTextColorParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
placeholderTextColorParser = do 
  name <- nameParserWithKeyword placeholderTextColorKeyword
  return (placeholderTextColorKeyword, StringVal name)

placeholderFontParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
placeholderFontParser = do 
  name <- nameParserWithKeyword placeholderFontKeyword
  return (placeholderFontKeyword, StringVal name)

backgroundColorParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
backgroundColorParser = do 
  name <- nameParserWithKeyword backgroundColorKeyword
  return (backgroundColorKeyword, StringVal name)

imageSourceParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
imageSourceParser = do 
  (_, filename) <- localizedKeyParserWithKeyword imageSourceKeyword
  return (imageSourceKeyword, StringVal filename)

constraintsParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
constraintsParser = do
  loneStringKeywordParser layoutKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  constraints <- Text.Parsec.try singleConstraintParser `sepEndBy` many (Text.Parsec.try indentedComment)
  modifyState reduceIndentationLevel
  return (constraintsKeyword, ConstraintsVal constraints)

singleConstraintParser :: GenParser Char GenericParserState OWAConstraint
singleConstraintParser = indentParser $ choice allConstraintParsers

allConstraintParsers :: [GenParser Char GenericParserState OWAConstraint]
allConstraintParsers = map Text.Parsec.try
  [heightConstraintParser,
  widthConstraintParser,
  alignTopConstraintParser,
  alignBottomConstraintParser,
  alignRightConstraintParser,
  alignLeftConstraintParser]

heightConstraintParser :: GenParser Char GenericParserState OWAConstraint
heightConstraintParser = do
  string heightKeyword
  spaceTabs
  (possibleViewName, possibleDimen) <- viewNameAndDimenOptionParser 
  return OWAConstraint {
    firstElementName = "",
    firstAttribute = Height,
    secondElementName = possibleViewName,
    secondAttribute = case possibleViewName of
      Just _ -> Just Height
      Nothing -> Nothing,
    multiplier = 1.0,
    constant = fromMaybe 0.0 possibleDimen
  }

widthConstraintParser :: GenParser Char GenericParserState OWAConstraint
widthConstraintParser = do
  string widthKeyword
  spaceTabs
  (possibleViewName, possibleDimen) <- viewNameAndDimenOptionParser 
  return OWAConstraint {
    firstElementName = "",
    firstAttribute = Width,
    secondElementName = possibleViewName,
    secondAttribute = case possibleViewName of
      Just _ -> Just Width
      Nothing -> Nothing,
    multiplier = 1.0,
    constant = fromMaybe 0.0 possibleDimen
  } 

alignTopConstraintParser :: GenParser Char GenericParserState OWAConstraint
alignTopConstraintParser = alignConstraintParser alignTopKeyword Top

alignBottomConstraintParser :: GenParser Char GenericParserState OWAConstraint
alignBottomConstraintParser = alignConstraintParser alignBottomKeyword Bottom

alignRightConstraintParser :: GenParser Char GenericParserState OWAConstraint
alignRightConstraintParser = alignConstraintParser alignRightKeyword RightSide

alignLeftConstraintParser :: GenParser Char GenericParserState OWAConstraint
alignLeftConstraintParser = alignConstraintParser alignLeftKeyword LeftSide

alignConstraintParser :: String -> OWALayoutAttribute -> GenParser Char GenericParserState OWAConstraint
alignConstraintParser keyword attribute = do
  string keyword
  many (oneOf " \t")
  (possibleViewName, possibleDimen) <- viewNameAndDimenOptionParser
  return OWAConstraint {
    firstElementName = "",
    firstAttribute = attribute,
    secondElementName = Just $ fromMaybe "Super" possibleViewName,
    secondAttribute = Just attribute,
    multiplier = 1.0,
    constant = fromMaybe 0.0 possibleDimen
  }

viewNameAndDimenOptionParser :: GenParser Char GenericParserState (Maybe String, Maybe Float)
viewNameAndDimenOptionParser = do
  possibleViewName <- optionMaybe nameParser
  case possibleViewName of
    Just vName -> do
      possibleSpace <- optionMaybe spaceTabs
      case possibleSpace of
        Just _ -> do
          possibleDimen <- optionMaybe parseFloat 
          singleTrailingComment
          return (Just vName, possibleDimen)
        Nothing -> do
          singleTrailingComment
          return (Just vName, Nothing)
    Nothing -> do
      possibleDimen <- optionMaybe parseFloat 
      singleTrailingComment
      return (Nothing, possibleDimen)

-------------------------------------------------------------------------------
-----------------------------------CONSTRUCTING VIEWS--------------------------
-------------------------------------------------------------------------------

splitAttrsAndConstraints :: [(ElementAttr, ElementVal)] -> (Map.Map String String, [OWAConstraint])
splitAttrsAndConstraints = splitAttrsAndConstraintsTail Map.empty []

splitAttrsAndConstraintsTail :: Map.Map String String -> [OWAConstraint] -> [(ElementAttr, ElementVal)] -> (Map.Map String String, [OWAConstraint])
splitAttrsAndConstraintsTail attrMap constraints [] = (attrMap, constraints)
splitAttrsAndConstraintsTail attrMap constraints ((elemAttr, elemVal):rest) = case elemVal of
  StringVal val -> splitAttrsAndConstraintsTail (Map.insert elemAttr val attrMap) constraints rest
  ConstraintsVal newConstraints -> splitAttrsAndConstraintsTail attrMap (constraints ++ newConstraints) rest

modifyConstraintsWithViewName :: String -> [OWAConstraint] -> [OWAConstraint]
modifyConstraintsWithViewName viewName = map
  (\c -> c {firstElementName = viewName})

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
  constraints = constraints
}
  where vType = case Map.lookup typeKeyword attrMap of
          Nothing -> fileName
          Just (TypeVal t) -> t
        (elems, constraints) = case Map.lookup elementsKeyword attrMap of
          Nothing -> ([], [])
          Just (ElementsVal (viewElems, constraints)) -> (viewElems, constraints)

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

heightKeyword :: String
heightKeyword = "Height"

widthKeyword :: String
widthKeyword = "Width" 

alignTopKeyword :: String
alignTopKeyword = "AlignTop"

alignBottomKeyword :: String
alignBottomKeyword = "AlignBottom"

alignRightKeyword :: String
alignRightKeyword = "AlignRight"

alignLeftKeyword :: String
alignLeftKeyword = "AlignLeft"
