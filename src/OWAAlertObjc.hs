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

objcHeaderFromAlerts :: String -> [OWAAlert] -> ObjcFile
objcHeaderFromAlerts categoryName alerts = ObjcFile
  [categoryCommentSection originalAlertTypeName categoryName True,
  uiKitImportsSection,
  alertHandlerTypedefSection, 
  CategoryInterfaceSection $ alertCategoryFromAlerts categoryName sortedAlerts]
    where sortedAlerts = sortBy sortAlertsByName alerts

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
methodBodyForAlert alert = 
  [constructorAssignment alert,
  ReturnStatement (Var "alert")]

constructorAssignment :: OWAAlert -> ObjcStatement
constructorAssignment alert = ExpressionStatement $ BinOp
  (VarDecl (PointerType originalAlertTypeName) "alert")
  Assign
  (MethodCall (Var originalAlertTypeName) alertControllerConstructor
    [localizedStringExpr $ alertTitle alert,
    localizedStringExpr $ alertMessage alert,
    Var "UIAlertControllerStyleAlert"])

alertControllerConstructor :: ObjcMethod
alertControllerConstructor = ObjcMethod {
  isStatic = True,
  nameIntro = "alertControllerWith",
  returnType = PointerType "UIAlertController",
  params = [ParamDef {
    paramTitle = "Title",
    paramType = PointerType "NSString",
    paramName = "title"
  }, ParamDef {
    paramTitle = "message",
    paramType = PointerType "NSString",
    paramName = "message"
  }, ParamDef {
    paramTitle = "preferredStyle",
    paramType = SimpleType "",
    paramName = "style"
  }],
  methodBody = []
}

localizedStringExpr :: String -> ObjcExpression
localizedStringExpr str = CFunctionCall "NSLocalizedString"
  [StringLit str,
  Var "nil"]

originalAlertTypeName :: String
originalAlertTypeName = "UIAlertController"

sortAlertsByName :: OWAAlert -> OWAAlert -> Ordering
sortAlertsByName alert1 alert2 = alertName alert1 `compare` alertName alert2
