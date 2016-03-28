{-|
Module      : OWALocalizedStringSet
Description : Wrapper model storing a map between localized keys and values,
  while also keeping track of the file name they came from.
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWALocalizedStringSet (
  OWALocalizedStringSet(..)
) where

import qualified Data.Map.Strict as Map

-- | OWALocalizedStringSet stores a name for the set, which is grabbed
-- from the filename and used as an identifier in the generated
-- strings file. It also stores a mapping between localized keys and values.
data OWALocalizedStringSet = OWALocalizedStringSet {
  setName :: String,
  setMap :: Map.Map String String
} deriving (Show, Eq)
