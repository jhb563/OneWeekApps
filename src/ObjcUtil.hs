{-|
Module      : ObjcUtil
Description : Utility module for common functions converting items to Objc Syntax
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module ObjcUtil (
  categoryCommentSection,
  uiKitImportsSection,
  categoryMImportsSection,
  categoryFromNamesAndMethodBuilder,
  localizedStringExpr  
) where

import OWAObjcAbSyn

-------------------------------------------------------------------------------
-------------------BLOCK COMMENT FOR TOP OF FILE-------------------------------
-------------------------------------------------------------------------------

-- | Takes strings for the original type name, the category name, and
-- a boolean signal for whether the comment is for a header or .m file.
-- Constructs the header comment which goes at the top of a category file.
categoryCommentSection :: String -> String -> Bool -> FileSection
categoryCommentSection originalTypeName categoryName isHeader = BlockCommentSection
  ["",
  categoryFileName originalTypeName categoryName isHeader,
  "MySampleApp",
  "",
  "Created By James Bowen 2/16/2016",
  "Copyright (c) 2016 One Week Apps. All Rights Reserved",
  ""]
  where ending = if isHeader then ".h" else ".m"

-------------------------------------------------------------------------------
-------------------IMPORT SECTIONS---------------------------------------------
-------------------------------------------------------------------------------

-- | Returns a imports section which simply imports the UIKit module
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

-------------------------------------------------------------------------------
-------------------LOCALIZED STRING HELPER-------------------------------------
-------------------------------------------------------------------------------

-- | Takes a string key and returns an expression featuring the string as a
-- localized key.
localizedStringExpr :: String -> ObjcExpression
localizedStringExpr str = CFunctionCall "NSLocalizedString"
  [StringLit str,
  Var "nil"]
