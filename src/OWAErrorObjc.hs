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

import Data.List
import ObjcUtil
import OWAError
import OWAObjcAbSyn
import qualified Data.Map.Strict as Map

type DomainMap = Map.Map String [OWAError]

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'objcHeaderFromErrors' takes a name for the new errors category, as well
-- as a list of error objects, and returns the structure for the category's
-- header file in Objective C
objcHeaderFromErrors :: String -> [OWAError] -> ObjcFile
objcHeaderFromErrors categoryName errors = ObjcFile
  [categoryCommentSection originalErrorTypeName categoryName True,
  foundationImportsSection]

-- | 'objcImplementationFromErrors' takes a name for the new errors category, as well
-- as a list of error objects, and returns the structure for the category's
-- implementation file in Objective C
objcImplementationFromErrors :: String -> [OWAError] -> ObjcFile
objcImplementationFromErrors categoryName errors = ObjcFile 
  [categoryCommentSection originalErrorTypeName categoryName False,
  categoryMImportsSection originalErrorTypeName categoryName]

--------------------------------------------------------------------------------
--------------------------TYPE KEYWORDS-----------------------------------------
--------------------------------------------------------------------------------

originalErrorTypeName :: String
originalErrorTypeName = "NSError"

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
sortErrorsByName error1 error2 = (errorName error1) `compare` (errorName error2)

