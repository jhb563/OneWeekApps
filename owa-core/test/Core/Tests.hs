module Main where

import System.Directory (getCurrentDirectory)

import Core.Tests.FileSearch.AppDirectory (runAppDirectorySearchTests)
import Core.Tests.FileSearch.AppInfo (runAppInfoSearchTests)
import Core.Tests.FileSearch.SourceFiles (runFileSearchTests)
import Core.Tests.FileSearch.Strings (runStringsSearchTests)
import Core.Tests.FileSearch.Views (runViewFileSearchTests)

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runAppDirectorySearchTests currentDirectory
  runAppInfoSearchTests currentDirectory
  runFileSearchTests currentDirectory
  runStringsSearchTests currentDirectory
  runViewFileSearchTests currentDirectory
