{-|
Module      : OWAColorParser
Description : Module for parsing values from a .colors file into OWAColor models
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAColorParser (
  parseColorsFromFile,
) where

import OWAColor
import System.Directory
import System.IO

parseColorsFromFile :: FilePath -> IO ([OWAColor])
parseColorsFromFile fPath = return []
