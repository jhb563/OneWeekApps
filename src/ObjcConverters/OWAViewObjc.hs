{-|
Module      : OWAViewObjc
Description : Module for Converting OWAViews to Objective C objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAViewObjc (
  objcHeaderFromView,
  objcImplementationFromView
) where

import Data.Maybe
import ObjcUtil
import OWAAppInfo
import OWAObjcAbSyn
import OWAElements
import OWAView

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'objcHeaderFromView' takes the app info and a view and returns
-- the structure for the view's header file.
objcHeaderFromView :: OWAAppInfo -> OWAView -> ObjcFile
objcHeaderFromView appInfo view = ObjcFile 
  [topCommentSection (vTy ++ ".h") appInfo,
  uiKitImportsSection,
  InterfaceSection vTy (Just "UIView") properties []]
    where vTy = viewType view
          subs = subviews view
          properties = map (propertyForSubview True) subs

-- | 'objcImplementationFromView' takes the app info and a view and returns
-- the structure for the view's implementation file.
objcImplementationFromView :: OWAAppInfo -> OWAView -> ObjcFile
objcImplementationFromView appInfo view = ObjcFile 
  [topCommentSection (vTy ++ ".m") appInfo,
  importsSection vTy (appPrefix appInfo),
  InterfaceSection vTy Nothing properties [],
  ImplementationSection vTy impSections]
    where vTy = viewType view
          subs = subviews view
          properties = map (propertyForSubview False) subs
          impSections = if not (null subs)
                          then [lifecycleSection, setupSection view, lazyGetterSection view]
                          else [lifecycleSection, setupSection view]

--------------------------------------------------------------------------------
--------------------------SECTION HELPERS---------------------------------------
--------------------------------------------------------------------------------

importsSection :: String -> String -> FileSection
importsSection viewType appPrefix = ImportsSection
  [FileImport (viewType ++ ".h"),
  FileImport colorFileName,
  FileImport fontFileName]
    where colorFileName = "UIColor+" ++ appPrefix ++ "Colors.h"
          fontFileName = "UIFont+" ++ appPrefix ++ "Fonts.h" 

lifecycleSection :: FileSection
lifecycleSection = MethodImplementationListSection (Just "Lifecycle") [initMethod]

setupSection :: OWAView -> FileSection
setupSection view = MethodImplementationListSection (Just "Setup") 
  [setupViewsMethod (subviews view), setupConstraintsMethod (constraints view)]

lazyGetterSection :: OWAView -> FileSection
lazyGetterSection view = MethodImplementationListSection (Just "Lazy Getters")
  (map lazyGetterForView (subviews view))

--------------------------------------------------------------------------------
--------------------------PROPERTIES--------------------------------------------
--------------------------------------------------------------------------------

propertyForSubview :: Bool -> OWAViewElement -> ObjcProperty
propertyForSubview isReadonly subview = ObjcProperty {
  propertyType = typeForElement subview,
  propertyAttributes = if isReadonly then ["nonatomic", "strong", "readonly"]
                        else ["nonatomic", "strong"],
  propertyName = nameForElement subview
}

typeNameForElement :: OWAViewElement -> String
typeNameForElement (LabelElement _) = "UILabel"
typeNameForElement (TextFieldElement _) = "UITextField"
typeNameForElement (ButtonElement _) = "UIButton"
typeNameForElement (ImageElement _) = "UIImageView"

typeForElement :: OWAViewElement -> ObjcType
typeForElement element = PointerType $ typeNameForElement element

nameForElement :: OWAViewElement -> String
nameForElement (LabelElement label) = labelName label
nameForElement (TextFieldElement textField) = textFieldName textField
nameForElement (ButtonElement button) = buttonName button
nameForElement (ImageElement image) = imageViewName image

selfExprForName :: String -> ObjcExpression
selfExprForName name = PropertyCall (Var "self") name

selfExprForElement :: OWAViewElement -> ObjcExpression
selfExprForElement element = selfExprForName (nameForElement element)

propExprForName :: String -> ObjcExpression
propExprForName name = Var $ '_':name

propExprForElement :: OWAViewElement -> ObjcExpression
propExprForElement element = propExprForName (nameForElement element)

--------------------------------------------------------------------------------
--------------------------METHOD HELPERS----------------------------------------
--------------------------------------------------------------------------------

initMethod :: ObjcMethod
initMethod = ObjcMethod {
  isStatic = False,
  nameIntro = "init",
  returnType = SimpleType "instancetype",
  params = [],
  methodBody = initBody
}
  where superCall = MethodCall (Var "super") libInit []
        superStatement = ExpressionStatement $ BinOp (Var "self") Assign superCall
        ifBody = [ExpressionStatement $ MethodCall (Var "self") (UserMethod setupViewsMethodBase) [],
                  ExpressionStatement $ MethodCall (Var "self") (UserMethod setupConstraintsMethodBase) []]
        ifBlock = IfBlock (Var "self") ifBody
        returnStatement = ReturnStatement $ Var "self"
        initBody = [superStatement, ifBlock, returnStatement]

setupViewsMethodBase :: ObjcMethod
setupViewsMethodBase = ObjcMethod {
  isStatic = False,
  nameIntro = "setupViews",
  returnType = SimpleType "void",
  params = [],
  methodBody = []
}

setupViewsMethod :: [OWAViewElement] -> ObjcMethod
setupViewsMethod subviews = setupViewsMethodBase {methodBody = setupViewsBody}
  where arrayDecl = ExpressionStatement $ BinOp
                      (VarDecl (PointerType "NSArray") "subviews")
                      Assign
                      (ArrayLit $ map selfExprForElement subviews)
        setMaskStatement = ExpressionStatement $ BinOp
                            (PropertyCall (Var "view") "translatesAutoresizingMaskIntoConstraints")
                            Assign
                            (Var "NO")
        addSubviewStatement = ExpressionStatement $ MethodCall (Var "self")
                                LibMethod {
                                  libNameIntro = "add",
                                  libParams = ["Subview"]
                                }
                                [Var "view"]
        forBlock = ForEachBlock
                    (VarDecl (PointerType "UIView") "view")
                    (Var "subviews")
                    [setMaskStatement, addSubviewStatement]
        setupViewsBody = [arrayDecl, forBlock]

setupConstraintsMethodBase :: ObjcMethod
setupConstraintsMethodBase = ObjcMethod {
  isStatic = False,
  nameIntro = "setupConstraints",
  returnType = SimpleType "void",
  params = [],
  methodBody = []
}
  
setupConstraintsMethod :: [OWAConstraint] -> ObjcMethod
setupConstraintsMethod constraints = setupConstraintsMethodBase {methodBody = setupConstraintsBody}
  where setupConstraintsBody = concatMap constraintStatements constraints

constraintStatements :: OWAConstraint -> [ObjcStatement]
constraintStatements constraint = [createConstraint, addConstraint]
  where firstName = firstElementName constraint
        firstAttr = firstAttribute constraint
        constraintName = firstName ++ show firstAttr
        secondItemExpr = case secondElementName constraint of
          Nothing -> Var "nil"
          Just "Super" -> Var "self"
          Just secondName -> PropertyCall (Var "self") secondName
        constraintInitialization = MethodCall 
          (Var "NSLayoutConstraint")
          LibMethod {
            libNameIntro = "constraintWith",
            libParams = ["Item", "attribute", "relatedBy", "toItem", "attribute", "multiplier", "constant"]
          }
          [PropertyCall (Var "self") firstName,
          Var $ objcStringForAttribute (Just firstAttr),
          Var "NSLayoutRelationEquality",
          secondItemExpr,
          Var $ objcStringForAttribute (secondAttribute constraint),
          FloatLit $ multiplier constraint,
          FloatLit $ constant constraint]
        createConstraint = ExpressionStatement $ BinOp
          (VarDecl (PointerType "NSLayoutConstraint") constraintName)
          Assign
          constraintInitialization
        addConstraint = ExpressionStatement $ MethodCall 
          (Var "self")
          LibMethod {
            libNameIntro = "add",
            libParams = ["Constraint"]
          }
          [Var constraintName]

objcStringForAttribute :: Maybe OWALayoutAttribute -> String
objcStringForAttribute Nothing = "NSLayoutAttributeNotAnAttribute"
objcStringForAttribute (Just attr) = "NSLayoutAttribute" ++ show attr

lazyGetterForView :: OWAViewElement -> ObjcMethod
lazyGetterForView element = ObjcMethod {
  isStatic = False,
  nameIntro = nameForElement element,
  returnType = typeForElement element,
  params = [],
  methodBody = lazyGetterBody element
}

lazyGetterBody :: OWAViewElement -> [ObjcStatement]
lazyGetterBody element = [lazyIfReturn, initialization] ++ customization ++ [rtrnStatement]
  where prop = propExprForElement element
        rtrnStatement = ReturnStatement prop
        lazyIfReturn = IfBlock prop [rtrnStatement]
        initialization = ExpressionStatement $ BinOp prop
          Assign
          (MethodCall (MethodCall (Var $ typeNameForElement element) libAlloc []) libInit [])
        customization = case element of
          (LabelElement label) -> labelCustomization label
          (TextFieldElement textField) -> textFieldCustomization textField
          (ButtonElement button) -> buttonCustomization button
          (ImageElement image) -> imageCustomization image

libAlloc :: CalledMethod
libAlloc = LibMethod {
  libNameIntro = "alloc",
  libParams = []
}

libInit :: CalledMethod
libInit = LibMethod {
  libNameIntro = "init",
  libParams = []
}

--------------------------------------------------------------------------------
--------------------------VIEW CUSTOMIZATION------------------------------------
--------------------------------------------------------------------------------

labelCustomization :: OWALabel -> [ObjcStatement]
labelCustomization label = textAssign:catMaybes [textColorAssign, fontAssign, backgroundAssign]
  where propExpr = propExprForName $ labelName label
        textAssign = valueAssignment propExpr "text" (localizedStringExpr $ labelText label)
        textColorAssign = case labelTextColorName label of
          Nothing -> Nothing
          Just color -> Just $ valueAssignment propExpr "textColor" (libMethodForColor color)
        fontAssign = case labelFontName label of
          Nothing -> Nothing
          Just font -> Just $ valueAssignment propExpr "font" (libMethodForFont font)
        backgroundAssign = case labelBackgroundColorName label of
          Nothing -> Nothing
          Just bColor -> Just $ valueAssignment propExpr "backgroundColor" (libMethodForColor bColor)

textFieldCustomization :: OWATextField -> [ObjcStatement]
textFieldCustomization textField = firstStatements ++ placeholders ++ maybeToList backgroundAssign 
  where propExpr = propExprForName $ textFieldName textField
        textAssign = case textFieldText textField of
          Nothing -> Nothing
          Just text -> Just $ valueAssignment propExpr "text" (localizedStringExpr text)
        textColorAssign = case textFieldColorName textField of
          Nothing -> Nothing
          Just textColor -> Just $ valueAssignment propExpr "textColor" (libMethodForColor textColor)
        fontAssign = case textFieldFontName textField of
          Nothing -> Nothing
          Just font -> Just $ valueAssignment propExpr "font" (libMethodForFont font)
        firstStatements = catMaybes [textAssign, textColorAssign, fontAssign]
        placeholders = placeholderStatements textField
        backgroundAssign = case textFieldBackgroundColorName textField of
          Nothing -> Nothing
          Just bColor -> Just $ valueAssignment propExpr "backgroundColor" (libMethodForColor bColor)

placeholderStatements :: OWATextField -> [ObjcStatement]
placeholderStatements textField = if noPlaceholders then [] else statements
  where pText =  textFieldPlaceholderText textField
        pColor = textFieldPlaceholderTextColorName textField
        pFont = textFieldPlaceholderFontName textField
        noPlaceholders = isNothing pText && isNothing pColor && isNothing pFont
        colorAttrPair = case pColor of
          Nothing -> Nothing
          Just color -> Just (Var "NSForegroundColorAttributeName", libMethodForColor color)
        fontAttrPair = case pFont of
          Nothing -> Nothing
          Just font -> Just (Var "NSFontAttributeName", libMethodForFont font)
        dictionary = DictionaryLit $ catMaybes [colorAttrPair, fontAttrPair]
        dictAssign = ExpressionStatement $ BinOp 
          (VarDecl (PointerType "NSDictionary") "placeholderAttributes")
          Assign
          dictionary
        initExpr = MethodCall (MethodCall (Var "NSAttributedString") libAlloc [])
          LibMethod {
            libNameIntro = "initWith",
            libParams = ["String", "attributes"]
          }
          [localizedStringExpr (fromMaybe "" pText), Var "placeholderAttributes"]
        placeholderInit = ExpressionStatement $ BinOp
          (VarDecl (PointerType "NSAttributedString") "placeholder")
          Assign
          initExpr
        propExpr = propExprForName $ textFieldName textField
        placeholderAssign = valueAssignment propExpr "attributedPlaceholder" (Var "placeholder")
        statements = [dictAssign, placeholderInit, placeholderAssign]

buttonCustomization :: OWAButton -> [ObjcStatement]
buttonCustomization button = textStatement:otherStatements
  where propExpr = propExprForName $ buttonName button
        textStatement = ExpressionStatement $ MethodCall propExpr
          LibMethod {
            libNameIntro = "set",
            libParams = ["Title", "forState"]
          }
          [localizedStringExpr $ buttonText button, Var "UIControlStateNormal"]
        textColorAssign = case buttonTextColorName button of
          Nothing -> Nothing
          Just color -> Just $ ExpressionStatement $ MethodCall propExpr
            LibMethod {
              libNameIntro = "set",
              libParams = ["TitleColor", "forState"]
            }
            [libMethodForColor color, Var "UIControlStateNormal"];
        fontAssign = case buttonFontName button of
          Nothing -> Nothing
          Just font -> Just $ valueAssignment (PropertyCall propExpr "titleLabel") "font" (libMethodForFont font)
        backgroundAssign = case buttonBackgroundColorName button of
          Nothing -> Nothing
          Just bColor -> Just $ valueAssignment propExpr "backgroundColor" (libMethodForColor bColor)
        otherStatements = catMaybes [textColorAssign, fontAssign, backgroundAssign]

imageCustomization :: OWAImageView -> [ObjcStatement]
imageCustomization image = [imageSourceAssign]
  where propExpr = propExprForName $ imageViewName image
        imageSourceAssign = valueAssignment propExpr "image" (libMethodForImage $ imageSourceName image)

libMethodForColor :: String -> ObjcExpression
libMethodForColor colorName = MethodCall
  (Var "UIColor")
  LibMethod {
    libNameIntro = colorName,
    libParams = []
  }
  []

libMethodForFont :: String -> ObjcExpression
libMethodForFont fontName = MethodCall
  (Var "UIFont")
  LibMethod {
    libNameIntro = fontName,
    libParams = []
  }
  []

libMethodForImage :: String -> ObjcExpression
libMethodForImage imageSrc = MethodCall
  (Var "UIImage")
  LibMethod {
    libNameIntro = "image",
    libParams = ["Named"]
  }
  [StringLit imageSrc]

valueAssignment :: ObjcExpression -> String -> ObjcExpression -> ObjcStatement
valueAssignment exp1 valueName newValue = ExpressionStatement $ BinOp
  (PropertyCall exp1 valueName) 
  Assign
  newValue
