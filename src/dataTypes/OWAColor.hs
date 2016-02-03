{-|
Module      : OWAColor
Description : Module for color model. All float values are expressed
  between 0.0 and 1.0
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAColor (
  OWAColor(..)
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
