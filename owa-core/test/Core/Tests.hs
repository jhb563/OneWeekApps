module Main where

import System.Directory (getCurrentDirectory)

import Core.Tests.FileSearch.AppDirectory (runAppDirectorySearchTests)
import Core.Tests.FileSearch.SourceFiles (runFileSearchTests)
import Core.Tests.FileSearch.AppInfo (runAppInfoSearchTests)

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runAppDirectorySearchTests currentDirectory
  runFileSearchTests currentDirectory
  runAppInfoSearchTests currentDirectory
