module Main where

import System.Directory (getCurrentDirectory)

import Core.Tests.AppInfoCLI.Basic (runAppInfoCLITests)
import Core.Tests.CodeType.Basic (runCodeTypeTests)
import Core.Tests.FileSearch.AppDirectory (runAppDirectorySearchTests)
import Core.Tests.FileSearch.AppInfo (runAppInfoSearchTests)
import Core.Tests.FileSearch.Models (runModelFileSearchTests)
import Core.Tests.FileSearch.SourceFiles (runFileSearchTests)
import Core.Tests.FileSearch.Strings (runStringsSearchTests)
import Core.Tests.FileSearch.Views (runViewFileSearchTests)
import Core.Tests.Integration.Version010.Basic (runV010IntegrationTests)
import Core.Tests.Integration.Version015.Basic (runV015IntegrationTests)
import Core.Tests.Integration.Version020.Basic (runV020IntegrationTests)
import Core.Tests.Integration.Version021.Basic (runV021IntegrationTests)
import Core.Tests.Integration.Version023.Basic (runV023IntegrationTests)
import Core.Tests.Integration.Version030.Basic (runV030IntegrationTests)
import Core.Tests.LazyGeneration.Basic (runLazyCodeGenerationTests)
import Core.Tests.XCode.Basic (runXCodeTests)

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  runAppDirectorySearchTests currentDirectory
  runAppInfoSearchTests currentDirectory
  runFileSearchTests currentDirectory
  runStringsSearchTests currentDirectory
  runViewFileSearchTests currentDirectory
  runModelFileSearchTests currentDirectory
  runV010IntegrationTests currentDirectory
  runV015IntegrationTests currentDirectory
  runV020IntegrationTests currentDirectory
  runV021IntegrationTests currentDirectory
  runV023IntegrationTests currentDirectory
  runV030IntegrationTests currentDirectory
  runLazyCodeGenerationTests currentDirectory 
  runAppInfoCLITests currentDirectory
  runCodeTypeTests currentDirectory
  runXCodeTests currentDirectory
