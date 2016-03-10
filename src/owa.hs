{-|
Module      : Main
Description : Main executable for OneWeekApps
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
Stability   : Stable
-}

module Main where

import System.Environment
import System.Directory
import OWALib

-- | 'main' runs the main progream
main :: IO ()
main = do
  currentFilePath <- getCurrentDirectory
  runOWA currentFilePath
