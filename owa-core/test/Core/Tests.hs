module Main where

import System.Directory (getCurrentDirectory)

import Core.Tests.FileSearch.AppDirectory (runAppDirectorySearchTests)
import Core.Tests.FileSearch.AppInfo (runAppInfoSearchTests)
import Core.Tests.FileSearch.SourceFiles (runFileSearchTests)
import Core.Tests.FileSearch.Strings (runStringsSearchTests)
import Core.Tests.FileSearch.Views (runViewFileSearchTests)
import Core.Tests.Integration.Version010.Basic (runV010IntegrationTests)
import Core.Tests.Integration.Version015.Basic (runV015IntegrationTests)

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runAppDirectorySearchTests currentDirectory
  runAppInfoSearchTests currentDirectory
  runFileSearchTests currentDirectory
  runStringsSearchTests currentDirectory
  runViewFileSearchTests currentDirectory
  runV010IntegrationTests currentDirectory
  runV015IntegrationTests currentDirectory
