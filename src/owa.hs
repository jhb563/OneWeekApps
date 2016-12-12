{-|
Module      : Main
Description : Main executable for OneWeekApps
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
Stability   : Stable
-}

module Main where

import System.Directory
import System.Environment
import System.IO

import OWALib

-- | 'main' runs the main progream
main :: IO ()
main = do
  currentFilePath <- getCurrentDirectory
  args <- getArgs
  runOWA stdin stdout currentFilePath args
