{-|
Module      : OWAColor
Description : Module for color model. All float values are expressed
  between 0.0 and 1.0
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAColor (
  OWAColor(..),
  colorFromTuple
) where

-- Simple RGBA color representation, with a name.
-- All values are expressed between 0.0 and 1.0
data OWAColor = OWAColor {
  colorName :: String,
  red :: Float,
  green :: Float,
  blue :: Float,
  alpha :: Float
} deriving (Show, Eq)

colorFromTuple :: (String, Float, Float, Float, Float) -> OWAColor
colorFromTuple (n, r, g, b, a) = OWAColor {
  colorName = n,
  red = adjustedRGBValue r,
  green = adjustedRGBValue g,
  blue = adjustedRGBValue b,
  alpha = a
}

adjustedRGBValue :: Float -> Float
adjustedRGBValue val
  | val > 255.0 = 1.0
  | val < 0.0 = 0.0
  | otherwise = val / 255.0
