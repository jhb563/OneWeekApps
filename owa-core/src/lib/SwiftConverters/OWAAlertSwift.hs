{-|
Module      : OWAAlertSwift
Description : Module for Converting OWAAlerts to Swift objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAAlertSwift (
  swiftExtensionFromAlerts
) where

import Data.List

import Model.OWAAppInfo
import Model.OWAAlert
import OWASwiftAbSyn
import OWASwiftUtil

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'swiftExtensionFromAlerts' takes the app info,
-- and a list of color objects and returns the structure for the extension's
-- Swift file
swiftExtensionFromAlerts :: OWAAppInfo -> [OWAAlert] -> SwiftFile
swiftExtensionFromAlerts appInfo alerts = SwiftFile
  [ extensionCommentSection filename appInfo
  , uiKitImportSection 
  , typeAliasSection
  , listSectionForAlerts (sort alerts) ]
  where 
    filename = alertExtensionFileName appInfo
    typeAliasSection = StatementListSection Nothing [typeAliasStatement]

--------------------------------------------------------------------------------
--------------------------EXTENSION CONSTRUCTION--------------------------------
--------------------------------------------------------------------------------

listSectionForAlerts :: [OWAAlert] -> FileSection
listSectionForAlerts alerts = ExtensionSection
  originalAlertTypeName
  maybeMethodSection
  where
    maybeMethodSection = if null alerts
      then []
      else [MethodImplementationListSection Nothing $ map methodForAlert alerts]

methodForAlert :: OWAAlert -> SwiftMethod
methodForAlert alert = SwiftMethod
  False
  ["class"]
  (alertName alert)
  (Just $ SimpleType originalAlertTypeName)
  (paramsForFormat format)
  (alertConstructor :
    actionStatementsForFormat format ++
    [ReturnStatement (Var "alert")])
  where
    format = alertButtonFormat alert
    alertConstructor = LetDecl "alert" $
      MethodCall 
      Nothing 
      alertConstructorMethod
      [ localizedStringForText (alertTitle alert)
      , localizedStringForText (alertMessage alert)
      , Var ".Alert" ]

paramsForFormat :: AlertButtonFormat -> [ParamDef]
paramsForFormat (DismissButton _) = []
paramsForFormat (NeutralButton _) = 
  [ ParamDef 
    { paramLabelName = Just "handler"
    , paramTitle = "handler"
    , paramType = SimpleType "AlertHandler" } ]
paramsForFormat (YesNoButtons _ _) =
  [ ParamDef
    { paramLabelName = Just "yesHandler"
    , paramTitle = "yesHandler"
    , paramType = SimpleType "AlertHandler" }
  , ParamDef
    { paramLabelName = Just "noHandler"
    , paramTitle = "noHandler"
    , paramType = SimpleType "AlertHandler" } ]

actionStatementsForFormat :: AlertButtonFormat -> [SwiftStatement]
actionStatementsForFormat (DismissButton name) = actionStatements name "dismissAction" ""
actionStatementsForFormat (NeutralButton name) = actionStatements name "neutralAction" "handler"
actionStatementsForFormat (YesNoButtons yesName noName) =
  actionStatements yesName "yesAction" "yesHandler" ++ actionStatements noName "noAction" "noHandler"

actionStatements :: String -> String -> String -> [SwiftStatement]
actionStatements buttonName actionName handlerName = 
  [ constructActionStatement
  , addActionStatement actionName]
  where
    handlerCall = ExpressionStatement $ MethodCall
      Nothing 
      (handlerMethod handlerName)
      []
    handlerClosure = Closure
      { closureParams = 
          [ ParamDef
            { paramLabelName = Just "alert"
            , paramTitle = "alert"
            , paramType = ExplicitType "UIAlertAction" } ] 
      , closureBody = [ handlerCall ] }
    handlerArg = if null handlerName
      then Var "nil"
      else ClosureExpr handlerClosure
    constructActionStatement = LetDecl actionName $
      MethodCall
        Nothing
        actionConstructorMethod
        [ localizedStringForText buttonName
        , Var ".Default"
        , handlerArg ]

addActionStatement :: String -> SwiftStatement
addActionStatement actionName = ExpressionStatement $ MethodCall
  (Just (Var "alert"))
  addActionMethod
  [Var actionName]

--------------------------------------------------------------------------------
--------------------------ALERT LIBRARY METHODS---------------------------------
--------------------------------------------------------------------------------

alertConstructorMethod :: CalledMethod
alertConstructorMethod = LibMethod
  { libMethodName = "UIAlertController"
  , libParams = map Just ["title", "message", "preferredStyle"] }

actionConstructorMethod :: CalledMethod
actionConstructorMethod = LibMethod
  { libMethodName = "UIAlertAction"
  , libParams = map Just ["title", "style", "handler"] }

addActionMethod :: CalledMethod
addActionMethod = LibMethod
  { libMethodName = "addAction"
  , libParams = [Nothing] }

handlerMethod :: String -> CalledMethod
handlerMethod hName = LibMethod
  { libMethodName = hName
  , libParams = [] }

--------------------------------------------------------------------------------
--------------------------TYPE ALIAS STATEMENT----------------------------------
--------------------------------------------------------------------------------

typeAliasStatement :: SwiftStatement
typeAliasStatement = TypeAliasDecl "AlertHandler" (FunctionType [] (SimpleType "Void"))

--------------------------------------------------------------------------------
--------------------------TYPE KEYWORDS-----------------------------------------
--------------------------------------------------------------------------------

originalAlertTypeName :: String
originalAlertTypeName = "UIAlertController"

alertExtensionFileName :: OWAAppInfo -> String
alertExtensionFileName appInfo = originalAlertTypeName ++
  ('+' : appPrefix appInfo ++ "Alerts.swift")
