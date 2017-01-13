{-|
Module      : Objc.Utils
Description : Utility module Objc.for common functions converting items to Objc Syntax
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Objc.Utils (
  topCommentSection,
  categoryCommentSection,
  foundationImportsSection,
  uiKitImportsSection,
  categoryMImportsSection,
  categoryFromNamesAndMethodBuilder,
  simpleCategoryInterface,
  simpleCategoryImplementation,
  localizedStringExpr  
) where

import Data.List.Split

import Model.OWAAppInfo
import Objc.AbSyn

-------------------------------------------------------------------------------
-------------------BLOCK COMMENT FOR TOP OF FILE-------------------------------
-------------------------------------------------------------------------------

-- | Takes a string for the file name and an app info object and constructs
-- the header comment which goes at the top of an objective C file.
topCommentSection :: String -> OWAAppInfo -> FileSection
topCommentSection filename appInfo = BlockCommentSection
  (definiteSection ++ possibleCompanySection)
  where dateCreated = dateCreatedString appInfo 
        createdString = "Created By " ++ authorName appInfo ++
          " " ++ dateCreatedString appInfo
        yearGiven = last $ splitOn "/" dateCreated
        yearCreated = if length yearGiven < 4 then "20" ++ yearGiven else yearGiven
        definiteSection = ["",
          filename,
          appName appInfo,
          "",
          createdString]
        possibleCompanySection = case companyName appInfo of
          Nothing -> [""]
          Just company -> ["Copyright (c) " ++ yearCreated ++ " " ++
            company ++ ". All Rights Reserved",
            ""]

-- | Takes an object describing the app info,
-- strings for the original type name, the category name, and
-- a boolean signal for whether the comment is for a header or .m file.
-- Constructs the header comment which goes at the top of a category file.
categoryCommentSection :: OWAAppInfo -> String -> String -> Bool -> FileSection
categoryCommentSection appInfo originalTypeName categoryName isHeader = topCommentSection
  (categoryFileName originalTypeName categoryName isHeader) appInfo

-------------------------------------------------------------------------------
-------------------IMPORT SECTIONS---------------------------------------------
-------------------------------------------------------------------------------

-- | Returns an imports section which simply imports the Foundation module
foundationImportsSection :: FileSection
foundationImportsSection = ImportsSection [ModuleImport "Foundation"]

-- | Returns an imports section which simply imports the UIKit module
uiKitImportsSection :: FileSection
uiKitImportsSection = ImportsSection [ModuleImport "UIKit"]

-- | Takes strings for the original type and category name of a category,
-- and returns a imports section importing the header file of that
-- category.
categoryMImportsSection :: String -> String -> FileSection
categoryMImportsSection originalTypeName categoryName = ImportsSection
  [FileImport $ categoryFileName originalTypeName categoryName True]

categoryFileName :: String -> String -> Bool -> String
categoryFileName originalTypeName categoryName isHeader = originalTypeName ++ ('+':categoryName) ++ ending
  where ending = if isHeader then ".h" else ".m"

-------------------------------------------------------------------------------
-------------------CATEGORY BUILDER--------------------------------------------
-------------------------------------------------------------------------------

-- | Takes the type and category names, a method for constructing ObjcMethod objects,
-- and a list of objects, and returns the category object.
categoryFromNamesAndMethodBuilder :: String -> String -> (a -> ObjcMethod) -> [a] -> Category
categoryFromNamesAndMethodBuilder typeName catName methodBuilder objects = Category {
  originalTypeName = typeName,
  categoryName = catName,
  categoryMethods = map methodBuilder objects
}

-- | Takes a category and returns the interface section where all method headers
-- are in a single block with no comment.
simpleCategoryInterface :: Category -> FileSection
simpleCategoryInterface category = InterfaceSection
  (originalTypeName category)
  Nothing
  (Just $ categoryName category)
  []
  (if null (categoryMethods category)
    then []
    else [MethodHeaderListSection Nothing (categoryMethods category)])

-- | Takes a category and returns the implementation section with no pragma marks
-- and all methods are in that section.
simpleCategoryImplementation :: Category -> FileSection
simpleCategoryImplementation category = ImplementationSection
  (originalTypeName category)
  (Just $ categoryName category)
  (if null (categoryMethods category)
    then []
    else [MethodImplementationListSection Nothing (categoryMethods category)])

-------------------------------------------------------------------------------
-------------------LOCALIZED STRING HELPER-------------------------------------
-------------------------------------------------------------------------------

-- | Takes a string key and returns an expression featuring the string as a
-- localized key.
localizedStringExpr :: String -> ObjcExpression
localizedStringExpr str = CFunctionCall "NSLocalizedString"
  [StringLit str,
  Var "nil"]
