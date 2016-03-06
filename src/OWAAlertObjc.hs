{-|
Module      : OWAAlertObjc
Description : Module for Converting OWAAlerts to Objective C objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAAlertObjc (
  objcHeaderFromAlerts,
  objcImplementationFromAlerts
) where

import Data.List
import ObjcUtil
import OWAAlert
import OWAObjcAbSyn

-- | 'objcHeaderFromAlerts' takes a name for the new alerts category, as well
-- as a list of alert objects, and returns the structure for the category's
-- header file in Objective C
objcHeaderFromAlerts :: String -> [OWAAlert] -> ObjcFile
objcHeaderFromAlerts categoryName alerts = ObjcFile
  [categoryCommentSection originalAlertTypeName categoryName True,
  uiKitImportsSection,
  alertHandlerTypedefSection, 
  CategoryInterfaceSection $ alertCategoryFromAlerts categoryName sortedAlerts]
    where sortedAlerts = sortBy sortAlertsByName alerts

-- | 'objcImplementationFromAlerts' takes a name for the new alerts category, as well
-- as a list of alert objects, and returns the structure for the category's
-- implementation file in Objective C
objcImplementationFromAlerts :: String -> [OWAAlert] -> ObjcFile
objcImplementationFromAlerts categoryName alerts = ObjcFile 
  [categoryCommentSection originalAlertTypeName categoryName False,
  categoryMImportsSection originalAlertTypeName categoryName,
  CategoryImplementationSection $ alertCategoryFromAlerts categoryName sortedAlerts]
    where sortedAlerts = sortBy sortAlertsByName alerts

alertHandlerTypedefSection :: FileSection
alertHandlerTypedefSection = ForwardDeclarationSection 
  [TypedefDecl (SimpleType "void") "AlertHandler" []]

alertCategoryFromAlerts :: String -> [OWAAlert] -> Category
alertCategoryFromAlerts categoryName = categoryFromNamesAndMethodBuilder
  originalAlertTypeName categoryName methodForAlert
 
methodForAlert :: OWAAlert -> ObjcMethod
methodForAlert alert = ObjcMethod {
  isStatic = True,
  nameIntro = intro,
  returnType = PointerType originalAlertTypeName,
  params = paramsForFormat $ alertButtonFormat alert,
  methodBody = methodBodyForAlert alert
} where hasHandler = case alertButtonFormat alert of
          (DismissButton _) -> False
          _ -> True
        name = alertName alert
        intro = if hasHandler then name ++ "With" else name

paramsForFormat :: AlertButtonFormat -> [ParamDef]
paramsForFormat (DismissButton _) = []
paramsForFormat (NeutralButton _) = [ParamDef {
    paramTitle = "Handler",
    paramType = SimpleType "AlertHandler",
    paramName = "handler"
  }]
paramsForFormat (YesNoButtons _ _) = [ParamDef {
    paramTitle = "YesHandler",
    paramType = SimpleType "AlertHandler",
    paramName = "yesHandler"
  }, ParamDef {
    paramTitle = "noHandler",
    paramType = SimpleType "AlertHandler",
    paramName = "noHandler"
  }]

methodBodyForAlert :: OWAAlert -> [ObjcStatement]
methodBodyForAlert alert = consStatement:(actionStatements ++ [returnStatement]) 
  where consStatement = constructorAssignment alert
        actionStatements = actionStatementsForFormat $ alertButtonFormat alert
        returnStatement = ReturnStatement (Var "alert")

constructorAssignment :: OWAAlert -> ObjcStatement
constructorAssignment alert = ExpressionStatement $ BinOp
  (VarDecl (PointerType originalAlertTypeName) "alert")
  Assign
  (MethodCall (Var originalAlertTypeName) alertControllerConstructor
    [localizedStringExpr $ alertTitle alert,
    localizedStringExpr $ alertMessage alert,
    Var "UIAlertControllerStyleAlert"])

alertControllerConstructor :: CalledMethod
alertControllerConstructor = LibMethod {
  libNameIntro = "alertControllerWith",
  libParams = ["Title", "message", "preferredStyle"]
}

localizedStringExpr :: String -> ObjcExpression
localizedStringExpr str = CFunctionCall "NSLocalizedString"
  [StringLit str,
  Var "nil"]

actionStatementsForFormat :: AlertButtonFormat -> [ObjcStatement]
actionStatementsForFormat (DismissButton name) = actionStatements name "dismissAction" ""
actionStatementsForFormat (NeutralButton name) = actionStatements name "neutralAction" "handler"
actionStatementsForFormat (YesNoButtons yesName noName) =
  actionStatements yesName "yesAction" "yesHandler" ++ actionStatements noName "noAction" "noHandler"

actionStatements :: String -> String -> String -> [ObjcStatement]
actionStatements buttonName actionName handlerName = 
  [ExpressionStatement $ BinOp (VarDecl (PointerType "UIAlertAction") actionName)
    Assign
    (constructActionExpr buttonName handlerName),
  addActionStatement actionName]

constructActionExpr :: String -> String -> ObjcExpression
constructActionExpr buttonName handlerName = MethodCall
  (Var "UIAlertAction")
  actionConstructorMethod
  [localizedStringExpr buttonName,
  Var "UIAlertActionStyleDefault",
  handlerExpr]
    where handlerExpr = if null handlerName then Var "nil"
                          else handlerBlock handlerName

addActionStatement :: String -> ObjcStatement
addActionStatement actionName = ExpressionStatement $
  MethodCall (Var "alert") addActionMethod [Var actionName]
  
addActionMethod :: CalledMethod 
addActionMethod = LibMethod {
  libNameIntro = "add",
  libParams = ["Action"]
}

actionConstructorMethod :: CalledMethod 
actionConstructorMethod = LibMethod {
  libNameIntro = "actionWith",
  libParams = ["Title", "style", "handler"]
}

handlerBlock :: String -> ObjcExpression
handlerBlock handlerName = VoidBlock
  [BlockParam {
    blockParamType = PointerType "UIAlertAction",
    blockParamName = "action"
  }]
  [IfBlock
    (Var handlerName)
    [ExpressionStatement $ CFunctionCall handlerName []]]
 
originalAlertTypeName :: String
originalAlertTypeName = "UIAlertController"

sortAlertsByName :: OWAAlert -> OWAAlert -> Ordering
sortAlertsByName alert1 alert2 = alertName alert1 `compare` alertName alert2
