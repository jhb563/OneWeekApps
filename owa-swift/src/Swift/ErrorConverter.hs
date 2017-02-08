{-|
Module      : Swift.ErrorConverter
Description : Module for Converting OWAErrors to Swift objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Swift.ErrorConverter (
  swiftExtensionFromErrors
) where

import Data.List

import Model.OWAAppInfo
import Model.OWAError
import Swift.AbSyn
import Swift.Utils

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'swiftExtensionFromErrors' takes the app info,
-- and a list of error objects and returns the structure for the extension's
-- Swift file
swiftExtensionFromErrors :: OWAAppInfo -> [OWAError] -> SwiftFile
swiftExtensionFromErrors appInfo errors = SwiftFile $
  extensionCommentSection filename appInfo
  : foundationImportSection
  : remainder
  where
    filename = errorExtensionFileName appInfo
    enumName = appPrefix appInfo ++ "ErrorsErrorCodes"
    sortedErrors = sort errors
    remainder = if null errors
      then [listSectionForErrors enumName []]
      else [ enumSectionForErrors enumName sortedErrors
           , listSectionForErrors enumName sortedErrors ]

--------------------------------------------------------------------------------
--------------------------EXTENSION CONSTRUCTION--------------------------------
--------------------------------------------------------------------------------

enumSectionForErrors :: String -> [OWAError] -> FileSection
enumSectionForErrors name' errors = EnumSection name' (SimpleType "Int") (map errorCode errors)

listSectionForErrors :: String -> [OWAError] -> FileSection
listSectionForErrors enumName errors = ExtensionSection
  originalErrorTypeName
  (map (sectionForDomain enumName) groupedErrors)
  where
    groupedErrors = groupBy (\e1 e2 -> errorDomain e1 == errorDomain e2) errors

sectionForDomain :: String -> [OWAError] -> FileSection
sectionForDomain _ [] = MethodImplementationListSection Nothing []
sectionForDomain enumName errors = MethodImplementationListSection
  (Just (errorDomain (head errors)))
  (map (methodForError enumName) errors)

methodForError :: String -> OWAError -> SwiftMethod
methodForError enumName err = SwiftMethod
  False
  ["class"]
  (errorName err)
  (Just $ SimpleType originalErrorTypeName)
  []
  [ReturnStatement (constructorExpr enumName err)]

constructorExpr :: String -> OWAError -> SwiftExpression
constructorExpr enumName err = MethodCall
  Nothing
  errorConstructor
  [ StringLit (errorDomain err)
  , PropertyCall (PropertyCall (Var enumName) (errorCode err)) "rawValue"
  , DictionaryLit 
    [(Var "NSLocalizedDescriptionKey", localizedStringForText (errorDescription err))] ]

--------------------------------------------------------------------------------
--------------------------LIBRARY METHODS---------------------------------------
--------------------------------------------------------------------------------

errorConstructor :: CalledMethod
errorConstructor = LibMethod
  { libMethodName = "NSError"
  , libParams = map Just ["domain", "code", "userInfo"] }

--------------------------------------------------------------------------------
--------------------------TYPE KEYWORDS-----------------------------------------
--------------------------------------------------------------------------------

originalErrorTypeName :: String
originalErrorTypeName = "NSError"

errorExtensionFileName :: OWAAppInfo -> String
errorExtensionFileName appInfo = originalErrorTypeName ++
  ('+' : appPrefix appInfo ++ "Errors.swift")
