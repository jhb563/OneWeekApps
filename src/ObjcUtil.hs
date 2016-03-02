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
  categoryFromNamesAndMethodBuilder
) where

import OWAObjcAbSyn

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

uiKitImportsSection :: FileSection
uiKitImportsSection = ImportsSection [ModuleImport "UIKit"]

categoryMImportsSection :: String -> String -> FileSection
categoryMImportsSection originalTypeName categoryName = ImportsSection
  [FileImport $ categoryFileName originalTypeName categoryName True]

categoryFileName :: String -> String -> Bool -> String
categoryFileName originalTypeName categoryName isHeader = originalTypeName ++ ('+':categoryName) ++ ending
  where ending = if isHeader then ".h" else ".m"

categoryFromNamesAndMethodBuilder :: String -> String -> (a -> ObjcMethod) -> [a] -> Category
categoryFromNamesAndMethodBuilder typeName catName methodBuilder objects = Category {
  originalTypeName = typeName,
  categoryName = catName,
  categoryMethods = map methodBuilder objects
}

