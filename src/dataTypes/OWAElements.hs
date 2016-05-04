{-|
Module      : OWAElements
Description : Module for models of view elements, such as labels, buttons, etc.
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAElements where

-- | OWALabel represents a label subview. Labels must be given a name
-- and text to enter. They can optionally be given the names of text color,
-- font, or background color elements.
data OWALabel = OWALabel {
  labelName :: String,
  labelText :: String,
  labelTextColorName :: Maybe String,
  labelFontName :: Maybe String,
  labelBackgroundColorName :: Maybe String
} deriving (Show, Eq)

-- | OWATextField represents a TextField subview. These must have a name
-- but otherwise all fields are optional. They encompass the color, text and font
-- of each of the entered text and the placeholder text, as well as the 
-- background color. 
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
 
-- | OWAButton represents a Button subview. These must be given a name and
-- text. They can optionally be given the names of objects for text color,
-- background color, and font. 
data OWAButton = OWAButton {
  buttonName :: String,
  buttonText :: String,
  buttonTextColorName :: Maybe String,
  buttonFontName :: Maybe String,
  buttonBackgroundColorName :: Maybe String
} deriving (Show, Eq) 

-- | OWAImageView represents an Image subview. These must have a name
-- and a sourcefile name.
data OWAImageView = OWAImageView {
  imageViewName :: String,
  imageSourceName :: String
} deriving (Show, Eq)
