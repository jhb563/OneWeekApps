module OWAFont where

data FontStyle = Thin | Light | Regular | Medium | Bold | Italic deriving (Show)

data OWAFont = OWAFont {
  fontName :: String,
  fontFamily :: String,
  fontSize :: Float,
  fontStyles :: [FontStyle]
} deriving (Show)
