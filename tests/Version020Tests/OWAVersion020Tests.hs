module Main where

import System.Directory

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  putStrLn currentDirectory
