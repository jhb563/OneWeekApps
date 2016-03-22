module OWAAppInfo (
  OWAAppInfo(..)
) where

data OWAAppInfo = OWAAppInfo {
  appName :: String,
  authorName :: String,
  dateCreatedString :: String,
  companyName :: Maybe String
}
