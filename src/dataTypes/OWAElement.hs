module OWAElement where

data OWALabel = OWALabel {
  labelName :: String,
  labelText :: String,
  textColorName :: Maybe String,
  fontName :: Maybe String,
  labelBackgroundColorName :: Maybe String
} deriving (Show, Eq)

data OWATextField = OWATextField {
  textFieldName :: String,
  textFieldText :: Maybe String,
  textFieldColorName :: Maybe String,
  textFieldFontName :: Maybe String,
  textFieldPlaceholderText :: Maybe String,
  textFieldPlaceholderTextColorName :: Maybe String,
  textFieldPlaceholderFontName :: Maybe String,
  textFieldBackgroundColorName :: Maybe String
} deriving (Show, Eq)
  
data OWAButton = OWAButton {
  buttonName :: String,
  buttonText :: String,
  buttonTextColor :: Maybe String,
  buttonFontName :: Maybe String,
  buttonBackgroundColorName :: Maybe String
} deriving (Show, Eq) 

data OWAImageView = OWAImageView {
  imageViewName :: String,
  sourceName :: String
} deriving (Show, Eq)
