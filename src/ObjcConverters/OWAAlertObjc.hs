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
import OWAAppInfo
import OWAAlert
import OWAObjcAbSyn

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'objcHeaderFromAlerts' takes the app info, a name for the new
-- alerts category, as well as a list of alert objects, and returns the 
-- structure for the category's header file in Objective C
objcHeaderFromAlerts :: OWAAppInfo -> [OWAAlert] -> ObjcFile
objcHeaderFromAlerts appInfo alerts = ObjcFile
  [categoryCommentSection appInfo originalAlertTypeName categoryName True,
  uiKitImportsSection,
  alertHandlerTypedefSection, 
  simpleCategoryInterface category]
    where (categoryName, category) = builderInfo appInfo alerts

-- | 'objcImplementationFromAlerts' takes the app info,
-- a name for the new alerts category, as well
-- as a list of alert objects, and returns the structure for the category's
-- implementation file in Objective C
objcImplementationFromAlerts :: OWAAppInfo -> [OWAAlert] -> ObjcFile
objcImplementationFromAlerts appInfo alerts = ObjcFile 
  [categoryCommentSection appInfo originalAlertTypeName categoryName False,
  categoryMImportsSection originalAlertTypeName categoryName,
  simpleCategoryImplementation category]
    where (categoryName, category) = builderInfo appInfo alerts

builderInfo :: OWAAppInfo -> [OWAAlert] -> (String, Category)
builderInfo appInfo alerts = (categoryName,
  alertCategoryFromAlerts categoryName sortedAlerts)
  where categoryName = appPrefix appInfo ++ "Alerts"
        sortedAlerts = sortBy sortAlertsByName alerts

--------------------------------------------------------------------------------
--------------------------CATEGORY CONSTRUCTION---------------------------------
--------------------------------------------------------------------------------

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
constructorAssignment alert = AssignStatement
  (VarDecl (PointerType originalAlertTypeName) "alert")
  (MethodCall (Var originalAlertTypeName) alertControllerConstructor
    [localizedStringExpr $ alertTitle alert,
    localizedStringExpr $ alertMessage alert,
    Var "UIAlertControllerStyleAlert"])

--------------------------------------------------------------------------------
--------------------------CONSTRUCTING ACTIONS----------------------------------
--------------------------------------------------------------------------------

actionStatementsForFormat :: AlertButtonFormat -> [ObjcStatement]
actionStatementsForFormat (DismissButton name) = actionStatements name "dismissAction" ""
actionStatementsForFormat (NeutralButton name) = actionStatements name "neutralAction" "handler"
actionStatementsForFormat (YesNoButtons yesName noName) =
  actionStatements yesName "yesAction" "yesHandler" ++ actionStatements noName "noAction" "noHandler"

actionStatements :: String -> String -> String -> [ObjcStatement]
actionStatements buttonName actionName handlerName = 
  [AssignStatement
    (VarDecl (PointerType alertTypeName) actionName)
    (constructActionExpr buttonName handlerName),
  addActionStatement actionName]

constructActionExpr :: String -> String -> ObjcExpression
constructActionExpr buttonName handlerName = MethodCall
  (Var alertTypeName)
  actionConstructorMethod
  [localizedStringExpr buttonName,
  Var "UIAlertActionStyleDefault",
  handlerExpr]
    where handlerExpr = if null handlerName then Var "nil"
                          else handlerBlock handlerName

addActionStatement :: String -> ObjcStatement
addActionStatement actionName = ExpressionStatement $
  MethodCall (Var "alert") addActionMethod [Var actionName]
  
handlerBlock :: String -> ObjcExpression
handlerBlock handlerName = VoidBlock
  [BlockParam {
    blockParamType = PointerType alertTypeName,
    blockParamName = "action"
  }]
  [IfBlock
    (Var handlerName)
    [ExpressionStatement $ CFunctionCall handlerName []]]
 
--------------------------------------------------------------------------------
--------------------------ALERT LIBRARY METHODS---------------------------------
--------------------------------------------------------------------------------

alertControllerConstructor :: CalledMethod
alertControllerConstructor = LibMethod {
  libNameIntro = "alertControllerWith",
  libParams = ["Title", "message", "preferredStyle"]
}

actionConstructorMethod :: CalledMethod 
actionConstructorMethod = LibMethod {
  libNameIntro = "actionWith",
  libParams = ["Title", "style", "handler"]
}

addActionMethod :: CalledMethod 
addActionMethod = LibMethod {
  libNameIntro = "add",
  libParams = ["Action"]
}

--------------------------------------------------------------------------------
--------------------------TYPE KEYWORDS-----------------------------------------
--------------------------------------------------------------------------------

originalAlertTypeName :: String
originalAlertTypeName = "UIAlertController"

alertTypeName :: String
alertTypeName = "UIAlertAction"

--------------------------------------------------------------------------------
--------------------------SORT HELPER-------------------------------------------
--------------------------------------------------------------------------------

sortAlertsByName :: OWAAlert -> OWAAlert -> Ordering
sortAlertsByName alert1 alert2 = alertName alert1 `compare` alertName alert2
