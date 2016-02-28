{-|
Module      : OWAFont
Description : Module for font model. Size value must be a positive float
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}
module OWAFont where

-- | Enumerated type for possible style attributes to add to a font. By
-- no means is this exhaustive, and may be modified to include more attributes
-- in the future.
data FontStyle = Thin | Light | Regular | Medium | Bold | Italic deriving (Show)

-- | Font model, which requires a name, family, size, and list of styles. 
-- Styles not covered by the FontStyle type above can be incorporated into
-- the fontFamily string, provided the fontStyles field is left blank.
data OWAFont = OWAFont {
  fontName :: String,
  fontFamily :: String,
  fontSize :: Float,
  fontStyles :: [FontStyle]
} deriving (Show)
