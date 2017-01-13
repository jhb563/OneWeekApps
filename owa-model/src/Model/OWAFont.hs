{-|
Module      : OWAFont
Description : Module for font model. Size value must be a positive float
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}
module Model.OWAFont where

-- | Enumerated type for possible style attributes to add to a font. By
-- no means is this exhaustive, and may be modified to include more attributes
-- in the future.
data FontStyle = Thin |
 Light |
 Regular |
 Medium |
 Bold |
 Italic
 deriving (Show, Read, Eq)

-- | Font model, which requires a name, family, size, and list of styles. 
-- Styles not covered by the FontStyle type above can be incorporated into
-- the fontFamily string, provided the fontStyles field is left blank.
data OWAFont = OWAFont {
  fontName :: String,
  fontFamily :: String,
  fontSize :: Float,
  fontStyles :: [FontStyle]
} deriving (Show, Eq)

-- | Ord instance, which just compares the names given to the fonts
instance Ord OWAFont where
  font1 `compare` font2 = fontName font1 `compare` fontName font2

-- | 'fullNameForFont' Takes a font and determines the full family name
-- to use based on the family name and styles list.
fullNameForFont :: OWAFont -> String
fullNameForFont font = case fontStyles font of
  [] -> fontFamily font
  styles -> fontFamily font ++ ('-':styleList)
    where styleList = foldl (\str style -> str ++ show style) "" styles
