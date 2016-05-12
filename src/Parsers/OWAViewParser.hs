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
    Right itemErrsOrView -> case itemErrsOrView of
      Left itemErrs -> return $ Left (map (attachFileName sourceName) itemErrs)
      Right view -> return $ Right view 

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
  textFieldElementParser, imageViewParser, customViewParser]

labelElementParser :: GenParser Char GenericParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
labelElementParser = do
  name <- nameParserWithKeyword labelKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  labelAttrsAndConstraints <- labelAttributeParser `sepEndBy` many (Text.Parsec.try indentedComment)
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
allLabelAttributeParsers = [textParser, 
  textColorParser, 
  fontParser, 
  backgroundColorParser,
  constraintsParser]

buttonElementParser :: GenParser Char GenericParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
buttonElementParser = do
  name <- nameParserWithKeyword buttonKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  buttonAttrsAndConstraints <- buttonAttributeParser `sepEndBy` many (Text.Parsec.try indentedComment)
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
allButtonAttributeParsers = [textParser, 
  textColorParser, 
  fontParser, 
  backgroundColorParser,
  constraintsParser]

textFieldElementParser :: GenParser Char GenericParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
textFieldElementParser = do
  name <- nameParserWithKeyword textFieldKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  textfieldAttrsAndConstraints <- textfieldAttributeParser `sepEndBy` many (Text.Parsec.try indentedComment)
  let (attrMap, constraints) = splitAttrsAndConstraints textfieldAttrsAndConstraints
  let modifiedConstraints = modifyConstraintsWithViewName name constraints
  let textfield = textFieldFromNameAndAttrs name attrMap
  modifyState reduceIndentationLevel
  return $ Right (TextFieldElement textfield, modifiedConstraints)

textfieldAttributeParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
textfieldAttributeParser = indentParser $ choice allTextfieldAttributeParsers

allTextfieldAttributeParsers :: [GenParser Char GenericParserState (ElementAttr, ElementVal)]
allTextfieldAttributeParsers = [textParser, 
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
  imageViewAttrsAndConstraints <- imageViewAttributeParser `sepEndBy` many (Text.Parsec.try indentedComment)
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
allImageViewAttributeParsers = [imageSourceParser,
  constraintsParser]

customViewParser :: GenParser Char GenericParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
customViewParser = do
  name <- nameParserWithKeyword customViewKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  customViewAttrsAndConstraints <- customViewAttributeParser `sepEndBy` many (Text.Parsec.try indentedComment)
  let (attrMap, constraints) = splitAttrsAndConstraints customViewAttrsAndConstraints
  let modifiedConstraints = modifyConstraintsWithViewName name constraints
  let maybeCustomViewRecord = customViewFromNameAndAttrs name attrMap
  many $ Text.Parsec.try indentedComment
  modifyState reduceIndentationLevel
  case maybeCustomViewRecord of
    Just customViewRecord -> return $ Right (CustomViewElement customViewRecord, modifiedConstraints)
    Nothing -> return $ Left ObjectError {
      fileName = "",
      itemName = name,
      -- Type is the only required custom view keyword
      missingRequiredAttributes = [typeKeyword]
    }

customViewAttributeParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
customViewAttributeParser = indentParser $ choice allCustomViewAttributeParsers

allCustomViewAttributeParsers :: [GenParser Char GenericParserState (ElementAttr, ElementVal)]
allCustomViewAttributeParsers = [viewTypeParser, constraintsParser]

textParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
textParser = do
  (attr, val) <- Text.Parsec.try $ localizedKeyParserWithKeyword textKeyword
  return (attr, StringVal val)
  
textColorParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
textColorParser = do 
  name <- Text.Parsec.try $ nameParserWithKeyword textColorKeyword
  return (textColorKeyword, StringVal name)

fontParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
fontParser = do 
  name <- Text.Parsec.try $ nameParserWithKeyword fontKeyword
  return (fontKeyword, StringVal name)

placeholderTextParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
placeholderTextParser = do
  (attr, val) <- Text.Parsec.try $ localizedKeyParserWithKeyword placeholderTextKeyword
  return (attr, StringVal val)

placeholderTextColorParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
placeholderTextColorParser = do 
  name <- Text.Parsec.try $ nameParserWithKeyword placeholderTextColorKeyword
  return (placeholderTextColorKeyword, StringVal name)

placeholderFontParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
placeholderFontParser = do 
  name <- Text.Parsec.try $ nameParserWithKeyword placeholderFontKeyword
  return (placeholderFontKeyword, StringVal name)

backgroundColorParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
backgroundColorParser = do 
  name <- Text.Parsec.try $ nameParserWithKeyword backgroundColorKeyword
  return (backgroundColorKeyword, StringVal name)

imageSourceParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
imageSourceParser = do 
  (_, filename) <- Text.Parsec.try $ localizedKeyParserWithKeyword imageSourceKeyword
  return (imageSourceKeyword, StringVal filename)

viewTypeParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
viewTypeParser = do
  (_, typeName) <- Text.Parsec.try $ variableNameParserWithKeyword typeKeyword
  return (typeKeyword, StringVal typeName)

constraintsParser :: GenParser Char GenericParserState (ElementAttr, ElementVal)
constraintsParser = do
  loneStringKeywordParser layoutKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  constraints <- singleConstraintParser `sepEndBy` many (Text.Parsec.try indentedComment)
  currentState <- getState
  if shouldUpdate currentState
    then modifyState setShouldNotUpdateLevel
    else modifyState reduceIndentationLevel
  return (constraintsKeyword, ConstraintsVal constraints)

singleConstraintParser :: GenParser Char GenericParserState OWAConstraint
singleConstraintParser = indentParser $ choice allConstraintParsers

allConstraintParsers :: [GenParser Char GenericParserState OWAConstraint]
allConstraintParsers = [heightConstraintParser,
  widthConstraintParser,
  alignTopConstraintParser,
  alignBottomConstraintParser,
  alignRightConstraintParser,
  alignLeftConstraintParser,
  belowConstraintParser,
  aboveConstraintParser,
  toRightConstraintParser,
  toLeftConstraintParser,
  centerXConstraintParser,
  centerYConstraintParser]

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
alignTopConstraintParser = matchingConstraintParser alignTopKeyword Top

alignBottomConstraintParser :: GenParser Char GenericParserState OWAConstraint
alignBottomConstraintParser = matchingConstraintParser alignBottomKeyword Bottom

alignRightConstraintParser :: GenParser Char GenericParserState OWAConstraint
alignRightConstraintParser = matchingConstraintParser alignRightKeyword RightSide

alignLeftConstraintParser :: GenParser Char GenericParserState OWAConstraint
alignLeftConstraintParser = matchingConstraintParser alignLeftKeyword LeftSide

belowConstraintParser :: GenParser Char GenericParserState OWAConstraint
belowConstraintParser = placementConstraintParser belowKeyword Top

aboveConstraintParser :: GenParser Char GenericParserState OWAConstraint
aboveConstraintParser = placementConstraintParser aboveKeyword Bottom

toRightConstraintParser :: GenParser Char GenericParserState OWAConstraint
toRightConstraintParser = placementConstraintParser toRightKeyword LeftSide

toLeftConstraintParser :: GenParser Char GenericParserState OWAConstraint
toLeftConstraintParser = placementConstraintParser toLeftKeyword RightSide

placementConstraintParser :: String -> OWALayoutAttribute -> GenParser Char GenericParserState OWAConstraint
placementConstraintParser keyword attribute = do
  Text.Parsec.try $ string keyword
  spaceTabs
  viewName <- nameParser
  possibleDimen <- optionMaybe (do
    spaceTabs
    parseFloat)
  singleTrailingComment
  let constant = fromMaybe 0.0 possibleDimen
  let actualConstant = possibleReverse * constant
  return OWAConstraint {
    firstElementName = "",
    firstAttribute = attribute,
    secondElementName = Just viewName,
    secondAttribute = reverseAttribute,
    multiplier = 1.0,
    constant = actualConstant
  }
    where reverseAttribute = case attribute of
                              Top -> Just Bottom
                              Bottom -> Just Top
                              RightSide -> Just LeftSide
                              LeftSide -> Just RightSide
                              _ -> Nothing
          possibleReverse = case attribute of
                              Bottom -> -1.0
                              RightSide -> -1.0 
                              _ -> 1.0

centerXConstraintParser :: GenParser Char GenericParserState OWAConstraint
centerXConstraintParser = matchingConstraintParser centerXKeyword CenterX

centerYConstraintParser :: GenParser Char GenericParserState OWAConstraint
centerYConstraintParser = matchingConstraintParser centerYKeyword CenterY

-- Used by alignment constraints and centering constraints. Applies when we parse a
-- keyword, both view name and dimen are optional, and both elements of the constraint
-- have the same attribute.
matchingConstraintParser :: String -> OWALayoutAttribute -> GenParser Char GenericParserState OWAConstraint
matchingConstraintParser keyword attribute = do
  Text.Parsec.try $ string keyword
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

customViewFromNameAndAttrs :: String -> Map.Map String String -> Maybe OWAViewRecord
customViewFromNameAndAttrs name attrMap = do
  typeName <- Map.lookup typeKeyword attrMap
  return OWAViewRecord {
    viewRecordName = name,
    viewRecordType = typeName
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

customViewKeyword :: String
customViewKeyword = "CustomView"

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

belowKeyword :: String
belowKeyword = "Below"

aboveKeyword :: String
aboveKeyword = "Above"

toRightKeyword :: String
toRightKeyword = "ToRightOf"

toLeftKeyword :: String
toLeftKeyword = "ToLeftOf"

centerXKeyword :: String
centerXKeyword = "CenterX"

centerYKeyword :: String
centerYKeyword = "CenterY"
