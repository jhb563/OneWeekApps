{-|
Module      : OWAViewSwift
Description : Module for Converting OWAViews to Swift objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAViewSwift (
  swiftFileFromView
) where

import Data.Maybe

import OWAAppInfo
import OWAElements
import OWASwiftAbSyn
import OWASwiftUtil
import OWAView

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'swiftFileFromView' takes the app info,
-- and a list of color objects and returns the structure for the extension's
-- Swift file
swiftFileFromView :: OWAAppInfo -> OWAView -> SwiftFile
swiftFileFromView appInfo view = SwiftFile 
  [ extensionCommentSection (typeName ++ ".swift") appInfo 
  , uiKitImportSection 
  , viewClassSection]
  where 
    typeName = viewType view
    lifecycleSection = MethodImplementationListSection (Just "Lifecycle Methods") 
      [initWithFrameMethod, initWithCoderMethod, initCommonMethod]
    setupSection = MethodImplementationListSection (Just "Setup Methods") 
      [setupViewsMethod view, setupConstraintsMethod view]
    lazyGetterSection = StatementListSection (Just "Lazy Getters")
      (map lazyGetterForElement (allChildViews view))
    viewClassSection = ClassSection typeName [originalViewType]
      [ lifecycleSection
      , setupSection
      , lazyGetterSection ]

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

initWithFrameMethod :: SwiftMethod
initWithFrameMethod = SwiftMethod
  { isInitializer = True
  , qualifiers = ["override"]
  , name = "init"
  , returnType = Nothing
  , params = [ ParamDef (Just "frame") "frame" (SimpleType "CGRect") ]
  , methodBody = [superInitCall, initCommonCall] }
  where
    superInitCall = ExpressionStatement $ MethodCall 
      (Just (Var "super")) 
      LibMethod
        { libMethodName = "init"
        , libParams = [Just "frame"] }
      [Var "frame"]
    initCommonCall = ExpressionStatement $
      MethodCall Nothing (UserMethod initCommonMethod) []

initWithCoderMethod :: SwiftMethod
initWithCoderMethod = SwiftMethod
  { isInitializer = True
  , qualifiers = ["required"]
  , name = "init?"
  , returnType = Nothing
  , params = [ ParamDef (Just "coder") "aDecoder" (SimpleType "NSCoder") ]
  , methodBody = [superInitCall, initCommonCall] }
  where
    superInitCall = ExpressionStatement $ MethodCall
      (Just (Var "super")) 
      LibMethod
        { libMethodName = "init"
        , libParams = [Just "coder"] }
      [Var "aDecoder"]
    initCommonCall = ExpressionStatement $
      MethodCall Nothing (UserMethod initCommonMethod) []

initCommonMethod :: SwiftMethod
initCommonMethod = SwiftMethod
  { isInitializer = False
  , qualifiers = []
  , name = "initCommon"
  , returnType = Nothing
  , params = []
  , methodBody = [setupViewsCall, setupConstraintsCall] }
  where
    setupViewsCall = ExpressionStatement $
      MethodCall Nothing (UserMethod setupViewsBase) []
    setupConstraintsCall = ExpressionStatement $
      MethodCall Nothing (UserMethod setupConstraintsBase) []

setupViewsBase :: SwiftMethod
setupViewsBase = SwiftMethod
  { isInitializer = False
  , qualifiers = []
  , name = "setupViews"
  , returnType = Nothing
  , params = []
  , methodBody = [] }

setupViewsMethod :: OWAView -> SwiftMethod
setupViewsMethod view = setupViewsBase {
    methodBody = superViewSection ++ otherContainerSections
  }
  where
    subs = subviews view
    superViewSection = setupViewsSectionForNameAndElements "" subs
    containers = concatMap searchSubviewsForContainers subs
    otherContainerSections = concatMap setupViewsSectionForElement containers

searchSubviewsForContainers :: OWAViewElement -> [OWAViewElement]
searchSubviewsForContainers containerElem@(ContainerViewElement container) = 
  containerElem : concatMap searchSubviewsForContainers (containerSubviews container)
searchSubviewsForContainers scrollElem@(ScrollViewElement scrollView) = 
  scrollElem : searchSubviewsForContainers (ContainerViewElement $ scrollViewContainer scrollView)
searchSubviewsForContainers _ = []

setupViewsSectionForElement :: OWAViewElement -> [SwiftStatement]
setupViewsSectionForElement (ContainerViewElement container) = setupViewsSectionForNameAndElements 
  (containerName container) 
  (containerSubviews container)
setupViewsSectionForElement (ScrollViewElement scrollView) = setupViewsSectionForNameAndElements
  (scrollViewName scrollView)
  [ContainerViewElement $ scrollViewContainer scrollView]

setupViewsSectionForNameAndElements :: String -> [OWAViewElement] -> [SwiftStatement]
setupViewsSectionForNameAndElements name elems = [assignOfArray, forBlock]
  where
    subviewsName = if null name then "subviews" else name ++ "Subviews"
    assignOfArray = LetDecl
      subviewsName
      (ArrayLit $ map (Var . nameForElement) elems)
    translatesStatement = AssignStatement
      (PropertyCall (Var "view") "translatesAutoresizingMaskIntoConstraints")
      (BoolLit False)
    addSubviewStatement = ExpressionStatement $ MethodCall
      (if null name then Nothing else Just (Var name))
      LibMethod { libMethodName = "addSubview", libParams = [Nothing]}
      [Var "view"]
    forBlock = ForEachBlock (Var "view") (Var subviewsName)
      [translatesStatement, addSubviewStatement]

setupConstraintsBase :: SwiftMethod
setupConstraintsBase = SwiftMethod
  { isInitializer = False
  , qualifiers = []
  , name = "setupConstraints"
  , returnType = Nothing
  , params = []
  , methodBody = [] }

setupConstraintsMethod :: OWAView -> SwiftMethod
setupConstraintsMethod view = setupConstraintsBase {
    methodBody = map constraintStatement (constraints view)
  }

constraintStatement :: OWAConstraint -> SwiftStatement
constraintStatement constraint = AssignStatement
  (PropertyCall constraintExpr "active")
  (BoolLit True)
  where
    constraintInitMethod = LibMethod
      { libMethodName = "NSLayoutConstraint"
      , libParams = map Just 
        ["item", "attribute", "relatedBy", "toItem", "attribute", "multiplier", "constant"] }
    secondItemExpr = case secondElementName constraint of
      Nothing -> Var "nil"
      Just "Super" -> Var "self"
      Just secondName -> Var secondName
    constraintExpr = MethodCall
      Nothing
      constraintInitMethod
      [ Var (firstElementName constraint)
      , swiftVarForAttribute (Just (firstAttribute constraint))
      , Var ".Equal"
      , secondItemExpr
      , swiftVarForAttribute (secondAttribute constraint)
      , FloatLit $ multiplier constraint
      , FloatLit $ constant constraint ]

--------------------------------------------------------------------------------
--------------------------LAZY GETTERS------------------------------------------
--------------------------------------------------------------------------------

allChildViews :: OWAView -> [OWAViewElement]
allChildViews view = concatMap allNestedSubviews (subviews view)

allNestedSubviews :: OWAViewElement -> [OWAViewElement]
allNestedSubviews containerElem@(ContainerViewElement container) = containerElem :
  concatMap allNestedSubviews (containerSubviews container)
allNestedSubviews scrollElem@(ScrollViewElement scrollView) = scrollElem : ContainerViewElement container :
  concatMap allNestedSubviews (containerSubviews container)
  where container = scrollViewContainer scrollView
allNestedSubviews otherElem = [otherElem]

lazyGetterForElement :: OWAViewElement -> SwiftStatement
lazyGetterForElement element = VarDecl 
  ["lazy"] 
  (nameForElement element)
  (ExplicitType typeName)
  (Just $ CalledClosure lazyClosure [])
  where
    typeName = typeNameForElement element
    initMethod = LibMethod {libMethodName = typeName, libParams = []}
    initStatement = LetDecl "v" (MethodCall Nothing initMethod [])
    returnStatement = ReturnStatement (Var "v")
    allStatements = initStatement : lazyGetterCustomization element ++ [returnStatement]
    lazyClosure = Closure [] allStatements

lazyGetterCustomization :: OWAViewElement -> [SwiftStatement]
lazyGetterCustomization (LabelElement label) = labelCustomization label
lazyGetterCustomization (ButtonElement button) = buttonCustomization button
lazyGetterCustomization (TextFieldElement textField) = textFieldCustomization textField
lazyGetterCustomization (ImageElement imageView) = imageCustomization imageView
lazyGetterCustomization _ = []

labelCustomization :: OWALabel -> [SwiftStatement]
labelCustomization label = textAssign:catMaybes [textColorAssign, fontAssign, backgroundAssign]
  where
    textAssign = propAssign "text" (localizedStringForText (labelText label))
    textColorAssign = case labelTextColorName label of
      Nothing -> Nothing
      Just color -> Just $ propAssign "textColor" (colorMethodCall color)
    fontAssign = case labelFontName label of
      Nothing -> Nothing
      Just font -> Just $ propAssign "font" (fontMethodCall font)
    backgroundAssign = case labelBackgroundColorName label of
      Nothing -> Nothing
      Just bColor -> Just $ propAssign "backgroundColor"
        (MethodCall (Just (Var "UIColor"))
          LibMethod { libMethodName = bColor, libParams = []}
          [])

buttonCustomization :: OWAButton -> [SwiftStatement]
buttonCustomization button = catMaybes [textStatement, textColorAssign, fontAssign, backgroundAssign, imageAssign]
  where
    textStatement = case buttonText button of
      Nothing -> Nothing
      Just text -> Just $ ExpressionStatement $ MethodCall 
        (Just (Var "v"))
        LibMethod { libMethodName = "setTitle", libParams = [Nothing, Just "forState"] }
        [localizedStringForText text, Var ".Normal"]
    textColorAssign = case buttonTextColorName button of
      Nothing -> Nothing
      Just textColor -> Just $ ExpressionStatement $ MethodCall
        (Just (Var "v"))
        LibMethod { libMethodName = "setTitleColor", libParams = [Nothing, Just "forState"] }
        [colorMethodCall textColor, Var ".Normal"]
    fontAssign = case buttonFontName button of
      Nothing -> Nothing
      Just font -> Just $ AssignStatement
        (PropertyCall (PropertyCall (Var "v") "titleLabel") "font")
        (fontMethodCall font)
    backgroundAssign = case buttonBackgroundColorName button of
      Nothing -> Nothing
      Just bColor -> Just $ propAssign "backgroundColor" (colorMethodCall bColor)
    imageAssign = case buttonBackgroundImageSourceName button of
      Nothing -> Nothing
      Just img -> Just $ ExpressionStatement $ MethodCall
        (Just (Var "v"))
        LibMethod { libMethodName = "setImage", libParams = [Nothing, Just "forState"] }
        [MethodCall Nothing 
            LibMethod { libMethodName = "UIImage", libParams = [Just "imageLiteral"]}
            [StringLit img],
          Var ".Normal"]
        
textFieldCustomization :: OWATextField -> [SwiftStatement]
textFieldCustomization textField = firstStatements ++ placeholders ++ maybeToList backgroundAssign 
  where
    textAssign = case textFieldText textField of
      Nothing -> Nothing
      Just text -> Just $ propAssign "text" (localizedStringForText text)
    textColorAssign = case textFieldColorName textField of
      Nothing -> Nothing
      Just textColor -> Just $ propAssign "textColor" (colorMethodCall textColor)
    fontAssign = case textFieldFontName textField of
      Nothing -> Nothing
      Just font -> Just $ propAssign "font" (fontMethodCall font)
    firstStatements = catMaybes [textAssign, textColorAssign, fontAssign]
    placeholders = placeholderStatements textField
    backgroundAssign = case textFieldBackgroundColorName textField of
      Nothing -> Nothing
      Just bColor -> Just $ propAssign "backgroundColor" (colorMethodCall bColor)

placeholderStatements :: OWATextField -> [SwiftStatement]
placeholderStatements textField = if noPlaceholders then [] else statements
  where
    pText = textFieldPlaceholderText textField
    pColor = textFieldPlaceholderTextColorName textField
    pFont = textFieldPlaceholderFontName textField
    noPlaceholders = isNothing pText && isNothing pColor && isNothing pFont
    colorAttrPair = case pColor of
      Nothing -> Nothing
      Just color -> Just (Var "NSForegroundColorAttributeName", colorMethodCall color)
    fontAttrPair = case pFont of
      Nothing -> Nothing
      Just font -> Just (Var "NSFontAttributeName", fontMethodCall font)
    dictionary = DictionaryLit $ catMaybes [colorAttrPair, fontAttrPair]
    dictAssign = LetDecl "placeholderAttributes" dictionary
    placeholderInit = LetDecl "attributedPlaceholder" $ MethodCall Nothing
      LibMethod { 
        libMethodName = "NSAttributedString", 
        libParams = map Just ["string", "attributes"] }
      [localizedStringForText (fromMaybe "" pText), Var "placeholderAttributes"]
    placeholderAssign = propAssign "attributedPlaceholder" (Var "attributedPlaceholder")
    statements = [dictAssign, placeholderInit, placeholderAssign]

imageCustomization :: OWAImageView -> [SwiftStatement]
imageCustomization image = [imageSourceAssign]
  where 
    imageSourceAssign = AssignStatement
      (PropertyCall (Var "v") "image")
      (MethodCall Nothing 
        LibMethod { libMethodName = "UIImage", libParams = [Just "imageLiteral"]}
        [StringLit (imageSourceName image)])

--------------------------------------------------------------------------------
--------------------------HELPERS-----------------------------------------------
--------------------------------------------------------------------------------

swiftVarForAttribute :: Maybe OWALayoutAttribute -> SwiftExpression
swiftVarForAttribute Nothing = Var ".NotAnAttribute"
swiftVarForAttribute (Just attr) = Var ('.' : show attr)

propAssign :: String -> SwiftExpression -> SwiftStatement
propAssign prop = AssignStatement (PropertyCall (Var "v") prop)

colorMethodCall :: String -> SwiftExpression
colorMethodCall color = MethodCall 
  (Just (Var "UIColor")) 
  LibMethod { libMethodName = color, libParams = [] }
  []

fontMethodCall :: String -> SwiftExpression
fontMethodCall font = MethodCall
  (Just (Var "UIFont"))
  LibMethod { libMethodName = font, libParams = []}
  []

--------------------------------------------------------------------------------
--------------------------KEYWORDS----------------------------------------------
--------------------------------------------------------------------------------

originalViewType :: String
originalViewType = "UIView"
