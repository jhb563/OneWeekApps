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
import OWAAppInfo
import OWAAlert
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
      else []

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
