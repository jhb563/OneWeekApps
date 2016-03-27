{-|
Module      : OWAStringsObjc
Description : Module for Converting OWALocalizedStringSets to Objective C objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAStringsObjc (
  objcStringsFileFromStringSets
) where

import Data.List
import ObjcUtil
import OWAAppInfo
import OWALocalizedStringSet
import OWAObjcAbSyn
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | Takes an appInfo object and a list of sets and returns a Objective C
-- file structure object representing the Localizable.strings file to be 
-- printed out.
objcStringsFileFromStringSets :: OWAAppInfo -> [OWALocalizedStringSet] -> ObjcFile
objcStringsFileFromStringSets appInfo stringSets = ObjcFile $
  topCommentSection "Localizable.strings" appInfo:map setSection sortedSets
    where sortedSets = sortBy sortStringSetsByName stringSets

--------------------------------------------------------------------------------
--------------------------STRING SECTIONS---------------------------------------
--------------------------------------------------------------------------------

setSection :: OWALocalizedStringSet -> FileSection
setSection stringSet = LocalizedStringListSection (setName stringSet)
  (Map.foldMapWithKey statementWithKeyAndValue $ setMap stringSet)

-- Put in a list to give it monoid property with fold
statementWithKeyAndValue :: String -> String -> [ObjcStatement]
statementWithKeyAndValue key value = [ExpressionStatement $
  BinOp
    (CStringLit key)
    Assign
    (CStringLit value)]

--------------------------------------------------------------------------------
--------------------------SORT HELPER-------------------------------------------
--------------------------------------------------------------------------------

sortStringSetsByName :: OWALocalizedStringSet -> OWALocalizedStringSet -> Ordering
sortStringSetsByName set1 set2 = setName set1 `compare` setName set2 
