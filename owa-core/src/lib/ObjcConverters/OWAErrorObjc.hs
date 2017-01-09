{-|
Module      : OWAErrorObjc
Description : Module for Converting OWAErrors to Objective C objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAErrorObjc (
  objcHeaderFromErrors,
  objcImplementationFromErrors
) where

import           Data.List
import qualified Data.Map.Strict as Map

import           Model.OWAAppInfo
import           Model.OWAError
import           OWAObjcAbSyn
import           OWAObjcUtil

type DomainMap = Map.Map String [OWAError]

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'objcHeaderFromErrors' takes the app info,
-- a name for the new errors category, as well
-- as a list of error objects, and returns the structure for the category's
-- header file in Objective C
objcHeaderFromErrors :: OWAAppInfo -> [OWAError] -> ObjcFile
objcHeaderFromErrors appInfo  errors = ObjcFile
  [categoryCommentSection appInfo originalErrorTypeName categoryName True,
  foundationImportsSection,
  InterfaceSection originalErrorTypeName Nothing (Just categoryName) [] sections]
    where category = categoryForErrors categoryName errors
          categoryName = appPrefix appInfo ++ "Errors"
          sections = methodHeaderSectionsForErrors errors

-- | 'objcImplementationFromErrors' takes the app info,
-- a name for the new errors category, as well
-- as a list of error objects, and returns the structure for the category's
-- implementation file in Objective C
objcImplementationFromErrors :: OWAAppInfo -> [OWAError] -> ObjcFile
objcImplementationFromErrors appInfo errors = if not (null errors)
  then ObjcFile [commentSection, includeSection, enumSect, implSection]
  else ObjcFile [commentSection, includeSection, implSection]
  where categoryName = appPrefix appInfo ++ "Errors" 
        commentSection = categoryCommentSection appInfo originalErrorTypeName categoryName False
        includeSection = categoryMImportsSection originalErrorTypeName categoryName
        enumSect = enumSection categoryName errors
        category = categoryForErrors categoryName errors
        sections = methodImplementationSectionsForErrors errors
        implSection = ImplementationSection
                        (originalTypeName category)
                        (Just categoryName)
                        sections

--------------------------------------------------------------------------------
--------------------------CONSTRUCTING CATEGORY---------------------------------
--------------------------------------------------------------------------------

categoryForErrors :: String -> [OWAError] -> Category
categoryForErrors categoryName = categoryFromNamesAndMethodBuilder 
  originalErrorTypeName 
  categoryName 
  methodFromError 

methodFromError :: OWAError -> ObjcMethod
methodFromError err = ObjcMethod {
  isStatic = True,
  nameIntro = errorName err,
  returnType = PointerType originalErrorTypeName,
  params = [],
  methodBody = [ReturnStatement $ returnExprForError err]
}

returnExprForError :: OWAError -> ObjcExpression
returnExprForError err = MethodCall
  (Var originalErrorTypeName)
  errorConstructorMethod
  [StringLit $ errorDomain err,
  Var $ errorCode err,
  localizedDictionaryExpr $ errorDescription err]

localizedDictionaryExpr :: String -> ObjcExpression
localizedDictionaryExpr description = DictionaryLit
  [(Var "NSLocalizedDescriptionKey", localizedStringExpr description)]

--------------------------------------------------------------------------------
--------------------------CONSTRUCTING FILE SECTIONS----------------------------
--------------------------------------------------------------------------------

enumSection :: String -> [OWAError] -> FileSection
enumSection categoryName errors = ForwardDeclarationSection [EnumDecl enumName codes]
  where enumName = categoryName ++ codesSuffix
        domains = sectionErrorsByDomain errors
        codes = map errorCode $ concatMap domainErrors domains

methodHeaderSectionsForErrors :: [OWAError] -> [FileSection]
methodHeaderSectionsForErrors errors = map headerSectionForDomain domains
  where domains = sectionErrorsByDomain errors

headerSectionForDomain :: OWAErrorDomain -> FileSection
headerSectionForDomain domain = MethodHeaderListSection
  (Just $ domainName domain)
  (map methodFromError $ domainErrors domain)

methodImplementationSectionsForErrors :: [OWAError] -> [FileSection]
methodImplementationSectionsForErrors errors = map mSectionForDomain domains
  where domains = sectionErrorsByDomain errors

mSectionForDomain :: OWAErrorDomain -> FileSection
mSectionForDomain domain = MethodImplementationListSection
  (Just $ domainName domain)
  (map methodFromError $ domainErrors domain)

--------------------------------------------------------------------------------
--------------------------ERROR LIBRARY METHOD----------------------------------
--------------------------------------------------------------------------------

errorConstructorMethod :: CalledMethod 
errorConstructorMethod = LibMethod {
  libNameIntro = "errorWith",
  libParams = ["Domain", "code", "userInfo"]
}

--------------------------------------------------------------------------------
--------------------------TYPE KEYWORDS-----------------------------------------
--------------------------------------------------------------------------------

originalErrorTypeName :: String
originalErrorTypeName = "NSError"

codesSuffix :: String
codesSuffix = "ErrorCodes"

--------------------------------------------------------------------------------
--------------------------ERROR SECTIONING--------------------------------------
--------------------------------------------------------------------------------

-- Divide errors into lists by domain.
sectionErrorsByDomain :: [OWAError] -> [OWAErrorDomain]
sectionErrorsByDomain errors = map sortErrorsInDomain domainTuples
  where domainMap = insertErrorsIntoDomainsTail errors Map.empty
        domainTuples = Map.toList domainMap

insertErrorsIntoDomainsTail :: [OWAError] -> DomainMap -> DomainMap
insertErrorsIntoDomainsTail [] domainMap = domainMap
insertErrorsIntoDomainsTail (err:errs) domainMap = insertErrorsIntoDomainsTail errs newMap
  where domainName = errorDomain err
        newMap = case Map.lookup domainName domainMap of
          Nothing -> Map.insert domainName [err] domainMap
          Just prevErrs -> Map.insert domainName (err:prevErrs) domainMap

sortErrorsInDomain :: (String, [OWAError]) -> OWAErrorDomain
sortErrorsInDomain (domainName, errors) = OWAErrorDomain {
  domainName = domainName,
  domainErrors = sortBy sortErrorsByName errors
}

-- Sort first by domain, then by name
sortErrorsByName :: OWAError -> OWAError -> Ordering
sortErrorsByName error1 error2 = errorName error1 `compare` errorName error2
