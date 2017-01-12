{-|
Module      : Objc.StringsConverter
Description : Module for Converting OWALocalizedStringSets to Objective C objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Objc.StringsConverter (
  objcStringsFileFromStringSets
) where

import           Data.List
import qualified Data.Map.Strict as Map

import           Model.OWAAppInfo
import           Model.OWALocalizedStringSet
import           Objc.AbSyn
import           Objc.Utils

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
statementWithKeyAndValue key value = [AssignStatement
  (CStringLit key)
  (CStringLit value)]

--------------------------------------------------------------------------------
--------------------------SORT HELPER-------------------------------------------
--------------------------------------------------------------------------------

sortStringSetsByName :: OWALocalizedStringSet -> OWALocalizedStringSet -> Ordering
sortStringSetsByName set1 set2 = setName set1 `compare` setName set2 
