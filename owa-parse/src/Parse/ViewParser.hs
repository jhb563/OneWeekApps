{-|
Module      : Parse.ViewParser
Description : Module for parsing a view from a .view file into a OWAView model
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Parse.ViewParser (
  parseViewFromFile
) where

import           Data.Either
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Error
import           Text.ParserCombinators.Parsec

import           Model.OWAElements
import           Model.OWAParseError
import           Parse.Utils
import           Model.OWAView

type ViewAttr = String
data ViewVal = TypeVal String |
  ElementsVal ([OWAViewElement], [OWAConstraint]) |
  ElementsErrs [OWAParseError]
type ElementAttr = String
data ElementVal = StringVal String |
  ConstraintsVal [OWAConstraint] |
  SubviewsVal ([OWAViewElement], [OWAConstraint]) |
  ErrorsVal [OWAParseError]
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
  ViewParserState {
    superViewStack = [],
    viewIndentationLevel = [],
    viewShouldUpdateIndent = False
  }
  sourceName

-------------------------------------------------------------------------------
----------------------VIEW STATE-----------------------------------------------
-------------------------------------------------------------------------------

data ViewParserState = ViewParserState {
  superViewStack :: [String],
  viewIndentationLevel :: [String],
  viewShouldUpdateIndent :: Bool
}

instance ParserState ViewParserState where
  currentIndentLevel = viewIndentationLevel
  shouldUpdateIndentLevel = viewShouldUpdateIndent
  addIndentationLevel newLevel currentState = currentState {
    viewIndentationLevel = viewIndentationLevel currentState ++ [newLevel],
    viewShouldUpdateIndent = False
  }
  reduceIndentationLevel currentState = currentState {
    viewIndentationLevel = init $ viewIndentationLevel currentState,
    viewShouldUpdateIndent = False
  }
  setShouldUpdateIndentLevel currentState = currentState {
    viewIndentationLevel = viewIndentationLevel currentState,
    viewShouldUpdateIndent = True
  }
  setShouldNotUpdateLevel currentState = currentState {
    viewShouldUpdateIndent = False
  }

pushSuperViewName :: String -> ViewParserState -> ViewParserState
pushSuperViewName newName currentState = currentState {
  superViewStack = newName : superViewStack currentState
}

popSuperViewName :: ViewParserState -> ViewParserState
popSuperViewName currentState = currentState {
  superViewStack = tail $ superViewStack currentState
}

-------------------------------------------------------------------------------
-----------------------------------PARSERS-------------------------------------
-------------------------------------------------------------------------------

viewParser :: String -> GenParser Char ViewParserState (Either [OWAParseError] OWAView)
viewParser fileName = do
  name <- nameParserWithKeyword viewKeyword
  modifyState $ pushSuperViewName defaultSuperViewKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  attrs <- attributeParser `sepEndBy` many (Text.Parsec.try indentedComment)
  let attrMap = Map.fromList attrs
  modifyState popSuperViewName
  modifyState reduceIndentationLevel
  case Map.lookup elementErrorsKeyword attrMap of
    Nothing -> return $ Right (viewFromNameFileAndAttrMap name fileName attrMap)
    Just (ElementsErrs es) -> return $ Left es

attributeParser :: GenParser Char ViewParserState (ViewAttr, ViewVal)
attributeParser = indentParser $ choice allAttrParsers

allAttrParsers :: [GenParser Char ViewParserState (ViewAttr, ViewVal)]
allAttrParsers = [typeParser, elementsParser]

typeParser :: GenParser Char ViewParserState (ViewAttr, ViewVal)
typeParser = do
  (_, typeName) <- variableNameParserWithKeyword typeKeyword 
  return (typeKeyword, TypeVal typeName)

elementsParser :: GenParser Char ViewParserState (ViewAttr, ViewVal)
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

elementParser :: GenParser Char ViewParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
elementParser = indentParser $ choice allElementParsers

allElementParsers :: [GenParser Char ViewParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))]
allElementParsers = [labelElementParser, buttonElementParser, 
  textFieldElementParser, imageViewParser, customViewParser,
  containerViewParser, scrollViewParser]

labelElementParser :: GenParser Char ViewParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
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

labelAttributeParser :: GenParser Char ViewParserState (ElementAttr, ElementVal)
labelAttributeParser = indentParser $ choice allLabelAttributeParsers

allLabelAttributeParsers :: [GenParser Char ViewParserState (ElementAttr, ElementVal)]
allLabelAttributeParsers = [textParser, 
  textColorParser, 
  fontParser, 
  backgroundColorParser,
  constraintsParser]

buttonElementParser :: GenParser Char ViewParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
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
      -- Text or ImageSrc is the only required keyword for buttons 
      missingRequiredAttributes = [textKeyword ++ " or " ++ imageSourceKeyword]
    }

buttonAttributeParser :: GenParser Char ViewParserState (ElementAttr, ElementVal)
buttonAttributeParser = indentParser $ choice allButtonAttributeParsers

allButtonAttributeParsers :: [GenParser Char ViewParserState (ElementAttr, ElementVal)]
allButtonAttributeParsers = [textParser, 
  textColorParser, 
  fontParser, 
  backgroundColorParser,
  constraintsParser,
  imageSourceParser]

textFieldElementParser :: GenParser Char ViewParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
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

textfieldAttributeParser :: GenParser Char ViewParserState (ElementAttr, ElementVal)
textfieldAttributeParser = indentParser $ choice allTextfieldAttributeParsers

allTextfieldAttributeParsers :: [GenParser Char ViewParserState (ElementAttr, ElementVal)]
allTextfieldAttributeParsers = [textParser, 
  textColorParser, 
  fontParser,
  placeholderTextParser, 
  placeholderTextColorParser, 
  placeholderFontParser,
  backgroundColorParser,
  constraintsParser]

imageViewParser :: GenParser Char ViewParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
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

imageViewAttributeParser :: GenParser Char ViewParserState (ElementAttr, ElementVal)
imageViewAttributeParser = indentParser $ choice allImageViewAttributeParsers

allImageViewAttributeParsers :: [GenParser Char ViewParserState (ElementAttr, ElementVal)]
allImageViewAttributeParsers = [imageSourceParser,
  constraintsParser]

customViewParser :: GenParser Char ViewParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
customViewParser = do
  name <- Text.Parsec.try $ nameParserWithKeyword customViewKeyword
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

customViewAttributeParser :: GenParser Char ViewParserState (ElementAttr, ElementVal)
customViewAttributeParser = indentParser $ choice allCustomViewAttributeParsers

allCustomViewAttributeParsers :: [GenParser Char ViewParserState (ElementAttr, ElementVal)]
allCustomViewAttributeParsers = [viewTypeParser, constraintsParser]

containerViewParser :: GenParser Char ViewParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
containerViewParser = do
  name <- nameParserWithKeyword containerViewKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  containerViewAttrsAndConstraints <- containerViewAttributeParser name `sepEndBy` many (Text.Parsec.try indentedComment)
  let (attrMap, constraints) = splitAttrsAndConstraints containerViewAttrsAndConstraints
  let (subElements, subConstraints, errors) = extractSubviewsAndErrors containerViewAttrsAndConstraints
  let modifiedConstraints = modifyConstraintsWithViewName name constraints
  let allConstraints = modifiedConstraints ++ subConstraints
  let containerView = containerViewFromNameSubviewsAndAttrs name subElements attrMap
  many $ Text.Parsec.try indentedComment
  modifyState reduceIndentationLevel
  return $ Right (ContainerViewElement containerView, allConstraints)

containerViewAttributeParser :: String -> GenParser Char ViewParserState (ElementAttr, ElementVal)
containerViewAttributeParser containerName = indentParser $ choice (allContainerViewAttributeParsers containerName)

allContainerViewAttributeParsers :: String -> [GenParser Char ViewParserState (ElementAttr, ElementVal)]
allContainerViewAttributeParsers containerName = [backgroundColorParser, 
  containerViewElementsParser containerName, 
  constraintsParser]

containerViewElementsParser :: String -> GenParser Char ViewParserState (ElementAttr, ElementVal)
containerViewElementsParser containerName = do
  modifyState $ pushSuperViewName containerName
  (viewAttr, viewVal) <- elementsParser
  modifyState popSuperViewName
  case viewVal of
    ElementsVal (elements, constraints) -> return (elementsKeyword, SubviewsVal (elements, constraints))
    ElementsErrs errs -> return (elementErrorsKeyword, ErrorsVal errs)

scrollViewParser :: GenParser Char ViewParserState (Either OWAParseError (OWAViewElement, [OWAConstraint]))
scrollViewParser = do
  name <- nameParserWithKeyword scrollViewKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  scrollViewAttrsAndConstraints <- scrollViewAttributeParser name `sepEndBy` many (Text.Parsec.try indentedComment)
  let (attrMap, constraints) = splitAttrsAndConstraints scrollViewAttrsAndConstraints
  let (subElements, subConstraints, errors) = extractSubviewsAndErrors scrollViewAttrsAndConstraints
  let modifiedConstraints = modifyConstraintsWithViewName name constraints
  let scrollView = scrollViewFromNameSubviewsAndAttrs name subElements attrMap
  let addedConstraintsOrFailure = addedScrollViewConstraints name (scrollDirection scrollView) modifiedConstraints
  many $ Text.Parsec.try indentedComment
  modifyState reduceIndentationLevel
  case addedConstraintsOrFailure of
    Left missingStrs -> return $ Left ObjectError {
      fileName = "",
      itemName = name,
      missingRequiredAttributes = missingStrs
    }
    Right addedConstraints -> do
      let allConstraints = modifiedConstraints ++ addedConstraints ++ subConstraints
      return $ Right (ScrollViewElement scrollView, allConstraints)


scrollViewAttributeParser :: String -> GenParser Char ViewParserState (ElementAttr, ElementVal)
scrollViewAttributeParser scrollViewName = indentParser $ choice (allScrollViewAttributeParsers scrollViewName)

allScrollViewAttributeParsers :: String -> [GenParser Char ViewParserState (ElementAttr, ElementVal)]
allScrollViewAttributeParsers scrollViewName = scrollDirectionParser : allContainerViewAttributeParsers (scrollViewName ++ "ContainerView")

scrollDirectionParser :: GenParser Char ViewParserState (ElementAttr, ElementVal)
scrollDirectionParser = do
  string scrollDirectionKeyword
  spaceTabs
  dirString <- choice $ map string [verticalKeyword, horizontalKeyword, bothKeyword]
  singleTrailingComment
  return (scrollDirectionKeyword, StringVal dirString)

textParser :: GenParser Char ViewParserState (ElementAttr, ElementVal)
textParser = do
  (attr, val) <- Text.Parsec.try $ localizedKeyParserWithKeyword textKeyword
  return (attr, StringVal val)
  
textColorParser :: GenParser Char ViewParserState (ElementAttr, ElementVal)
textColorParser = do 
  name <- Text.Parsec.try $ nameParserWithKeyword textColorKeyword
  return (textColorKeyword, StringVal name)

fontParser :: GenParser Char ViewParserState (ElementAttr, ElementVal)
fontParser = do 
  name <- Text.Parsec.try $ nameParserWithKeyword fontKeyword
  return (fontKeyword, StringVal name)

placeholderTextParser :: GenParser Char ViewParserState (ElementAttr, ElementVal)
placeholderTextParser = do
  (attr, val) <- Text.Parsec.try $ localizedKeyParserWithKeyword placeholderTextKeyword
  return (attr, StringVal val)

placeholderTextColorParser :: GenParser Char ViewParserState (ElementAttr, ElementVal)
placeholderTextColorParser = do 
  name <- Text.Parsec.try $ nameParserWithKeyword placeholderTextColorKeyword
  return (placeholderTextColorKeyword, StringVal name)

placeholderFontParser :: GenParser Char ViewParserState (ElementAttr, ElementVal)
placeholderFontParser = do 
  name <- Text.Parsec.try $ nameParserWithKeyword placeholderFontKeyword
  return (placeholderFontKeyword, StringVal name)

backgroundColorParser :: GenParser Char ViewParserState (ElementAttr, ElementVal)
backgroundColorParser = do 
  name <- Text.Parsec.try $ nameParserWithKeyword backgroundColorKeyword
  return (backgroundColorKeyword, StringVal name)

imageSourceParser :: GenParser Char ViewParserState (ElementAttr, ElementVal)
imageSourceParser = do 
  (_, filename) <- Text.Parsec.try $ localizedKeyParserWithKeyword imageSourceKeyword
  return (imageSourceKeyword, StringVal filename)

viewTypeParser :: GenParser Char ViewParserState (ElementAttr, ElementVal)
viewTypeParser = do
  (_, typeName) <- Text.Parsec.try $ variableNameParserWithKeyword typeKeyword
  return (typeKeyword, StringVal typeName)

constraintsParser :: GenParser Char ViewParserState (ElementAttr, ElementVal)
constraintsParser = do
  loneStringKeywordParser layoutKeyword
  modifyState setShouldUpdateIndentLevel
  many $ Text.Parsec.try indentedComment
  constraints <- singleConstraintParser `sepEndBy` many (Text.Parsec.try indentedComment)
  currentState <- getState
  if viewShouldUpdateIndent currentState
    then modifyState setShouldNotUpdateLevel
    else modifyState reduceIndentationLevel
  return (constraintsKeyword, ConstraintsVal constraints)

singleConstraintParser :: GenParser Char ViewParserState OWAConstraint
singleConstraintParser = indentParser $ choice allConstraintParsers

allConstraintParsers :: [GenParser Char ViewParserState OWAConstraint]
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

heightConstraintParser :: GenParser Char ViewParserState OWAConstraint
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

widthConstraintParser :: GenParser Char ViewParserState OWAConstraint
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

alignTopConstraintParser :: GenParser Char ViewParserState OWAConstraint
alignTopConstraintParser = matchingConstraintParser alignTopKeyword Top

alignBottomConstraintParser :: GenParser Char ViewParserState OWAConstraint
alignBottomConstraintParser = matchingConstraintParser alignBottomKeyword Bottom

alignRightConstraintParser :: GenParser Char ViewParserState OWAConstraint
alignRightConstraintParser = matchingConstraintParser alignRightKeyword RightSide

alignLeftConstraintParser :: GenParser Char ViewParserState OWAConstraint
alignLeftConstraintParser = matchingConstraintParser alignLeftKeyword LeftSide

belowConstraintParser :: GenParser Char ViewParserState OWAConstraint
belowConstraintParser = placementConstraintParser belowKeyword Top

aboveConstraintParser :: GenParser Char ViewParserState OWAConstraint
aboveConstraintParser = placementConstraintParser aboveKeyword Bottom

toRightConstraintParser :: GenParser Char ViewParserState OWAConstraint
toRightConstraintParser = placementConstraintParser toRightKeyword LeftSide

toLeftConstraintParser :: GenParser Char ViewParserState OWAConstraint
toLeftConstraintParser = placementConstraintParser toLeftKeyword RightSide

placementConstraintParser :: String -> OWALayoutAttribute -> GenParser Char ViewParserState OWAConstraint
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

centerXConstraintParser :: GenParser Char ViewParserState OWAConstraint
centerXConstraintParser = matchingConstraintParser centerXKeyword CenterX

centerYConstraintParser :: GenParser Char ViewParserState OWAConstraint
centerYConstraintParser = matchingConstraintParser centerYKeyword CenterY

-- Used by alignment constraints and centering constraints. Applies when we parse a
-- keyword, both view name and dimen are optional, and both elements of the constraint
-- have the same attribute.
matchingConstraintParser :: String -> OWALayoutAttribute -> GenParser Char ViewParserState OWAConstraint
matchingConstraintParser keyword attribute = do
  Text.Parsec.try $ string keyword
  many (oneOf " \t")
  (possibleViewName, possibleDimen) <- viewNameAndDimenOptionParser
  currentState <- getState
  let currentSuper = head $ superViewStack currentState 
  return OWAConstraint {
    firstElementName = "",
    firstAttribute = attribute,
    secondElementName = Just $ fromMaybe currentSuper possibleViewName,
    secondAttribute = Just attribute,
    multiplier = 1.0,
    constant = fromMaybe 0.0 possibleDimen
  }

viewNameAndDimenOptionParser :: GenParser Char ViewParserState (Maybe String, Maybe Float)
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
  _ -> splitAttrsAndConstraintsTail attrMap constraints rest

extractSubviewsAndErrors :: [(ElementAttr, ElementVal)] -> ([OWAViewElement], [OWAConstraint], [OWAParseError])
extractSubviewsAndErrors = extractSubviewsAndErrorsTail ([], [], [])

extractSubviewsAndErrorsTail :: ([OWAViewElement], [OWAConstraint], [OWAParseError]) -> [(ElementAttr, ElementVal)] -> ([OWAViewElement], [OWAConstraint], [OWAParseError])
extractSubviewsAndErrorsTail elemsTuple [] = elemsTuple
extractSubviewsAndErrorsTail (elems, constraints, errs) ((attr, val):rest) = extractSubviewsAndErrorsTail (elems', constraints', errs') rest
  where (elems', constraints', errs') = case val of
                  SubviewsVal (subviews, newConstraints) -> (elems ++ subviews, constraints ++ newConstraints, errs)
                  ErrorsVal newErrors -> (elems, constraints, errs ++ newErrors)
                  _ -> (elems, constraints, errs)

modifyConstraintsWithViewName :: String -> [OWAConstraint] -> [OWAConstraint]
modifyConstraintsWithViewName viewName = map
  (\c -> c {firstElementName = viewName})

addedScrollViewConstraints :: String -> OWAScrollDirection -> [OWAConstraint] -> Either [String] [OWAConstraint]
addedScrollViewConstraints name direction constraints = case direction of
  Vertical -> case missingLeftRightStrings of
    [] -> Right $ topBottomEqualConstraints ++ map switchElemName leftRightOutsideConstraints
    missingStrs -> Left missingStrs
  Horizontal -> case missingTopBottomStrings of
    [] -> Right $ map switchElemName topBottomOutsideConstraints ++ leftRightEqualConstraints
    missingStrs -> Left missingStrs 
  Both -> Right $ topBottomEqualConstraints ++ leftRightEqualConstraints
  where containerName = name ++ scrollViewContainerExtension
        topBottomEqualConstraints = [containerEqualityConstraint name Top, containerEqualityConstraint name Bottom]
        leftRightEqualConstraints = [containerEqualityConstraint name LeftSide, containerEqualityConstraint name RightSide]
        scrollViewTopConstraint = findMatchingScrollViewConstraint name Top constraints
        scrollViewBottomConstraint = findMatchingScrollViewConstraint name Bottom constraints
        scrollViewLeftSideConstraint = findMatchingScrollViewConstraint name LeftSide constraints
        scrollViewRightSideConstraint = findMatchingScrollViewConstraint name RightSide constraints
        switchElemName c = c {firstElementName = containerName}
        (missingTopBottomStrings, topBottomOutsideConstraints) = partitionEithers [scrollViewTopConstraint, scrollViewBottomConstraint]
        (missingLeftRightStrings, leftRightOutsideConstraints) = partitionEithers [scrollViewLeftSideConstraint, scrollViewRightSideConstraint]

containerEqualityConstraint :: String -> OWALayoutAttribute -> OWAConstraint
containerEqualityConstraint name attribute = OWAConstraint {
  firstElementName = name ++ scrollViewContainerExtension,
  firstAttribute = attribute,
  secondElementName = Just name,
  secondAttribute = Just attribute,
  multiplier = 1.0,
  constant = 0
}

findMatchingScrollViewConstraint :: String -> OWALayoutAttribute -> [OWAConstraint] -> Either String OWAConstraint
findMatchingScrollViewConstraint name attr constraints = if not (null results) then Right (head results) else Left (show attr ++ "Constraint")
  where results = filter
                    (\c -> firstElementName c == name && firstAttribute c == attr)
                    constraints

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
buttonFromNameAndAttrs name attrMap = if isNothing text && isNothing imgSourceName
  then Nothing
  else Just OWAButton {
    buttonName = name,
    buttonText = text,
    buttonTextColorName = Map.lookup textColorKeyword attrMap,
    buttonFontName = Map.lookup fontKeyword attrMap,
    buttonBackgroundColorName = Map.lookup backgroundColorKeyword attrMap,
    buttonBackgroundImageSourceName = imgSourceName
  }
    where text = Map.lookup textKeyword attrMap
          imgSourceName = Map.lookup imageSourceKeyword attrMap

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

containerViewFromNameSubviewsAndAttrs :: String -> [OWAViewElement] -> Map.Map String String -> OWAContainer
containerViewFromNameSubviewsAndAttrs name subElements attrMap = OWAContainer {
  containerName = name,
  containerBackgroundColorName = Map.lookup backgroundColorKeyword attrMap,
  containerSubviews = subElements
}

scrollViewFromNameSubviewsAndAttrs :: String -> [OWAViewElement] -> Map.Map String String -> OWAScrollView
scrollViewFromNameSubviewsAndAttrs name subElements attrMap = OWAScrollView {
  scrollViewName = name,
  scrollViewBackgroundColorName = Map.lookup backgroundColorKeyword attrMap,
  scrollDirection = direction,
  scrollViewContainer = OWAContainer {
    containerName = name ++ scrollViewContainerExtension,
    containerBackgroundColorName = Nothing,
    containerSubviews = subElements
  }
}
  where direction = case Map.lookup scrollDirectionKeyword attrMap of
                      Just "Horizontal" -> Horizontal
                      Just "Both" -> Both
                      _ -> Vertical

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

defaultSuperViewKeyword :: String
defaultSuperViewKeyword = "Super"

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

containerViewKeyword :: String
containerViewKeyword = "ContainerView"

scrollViewKeyword :: String
scrollViewKeyword = "ScrollView"

scrollViewContainerExtension :: String
scrollViewContainerExtension = "ContainerView"

scrollDirectionKeyword :: String
scrollDirectionKeyword = "ScrollDirection"

verticalKeyword :: String
verticalKeyword = "Vertical"

horizontalKeyword :: String
horizontalKeyword = "Horizontal"

bothKeyword :: String
bothKeyword = "Both"

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
