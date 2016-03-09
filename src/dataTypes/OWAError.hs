{-|
Module      : OWAError
Description : Module for error model.
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAError where

-- | Typedef for a localized key
type LocalizedKey = String

-- | Model for an error domain, which is a collection of errors.
data OWAErrorDomain = OWAErrorDomain {
  domainName :: String,
  domainCodes :: [String]
}

-- | Error model, which requires a name, domain, code and localized description
data OWAError = OWAError {
  errorName :: String,
  errorDomain :: String,
  errorCode :: String,
  errorDescription :: LocalizedKey
} deriving (Show, Eq)
