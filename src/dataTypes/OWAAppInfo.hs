{-|
Module      : OWAAppInfo
Description : Module for "App Info" model, storing default
  information about the app.
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAAppInfo (
  OWAAppInfo(..)
) where

-- | This model stores the app name and prefix, the author's name,
-- the company name (if given) and the date created for
-- the project. These items are used to populate header
-- comments in Objective C.
data OWAAppInfo = OWAAppInfo {
  appName :: String,
  appPrefix :: String,
  authorName :: String,
  dateCreatedString :: String,
  companyName :: Maybe String
} deriving (Show, Eq)
