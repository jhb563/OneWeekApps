-- OWALib will expose a method:
-- runOWA :: FilePath -> [String] -> IO ()
-- which will run the One Week Apps program, searching for
-- an app directory, finding relevant files, and producing
-- the correct output files. 
module Core.Tests.Integration.Version010.Basic (
  runV010IntegrationTests
) where

import Core.Tests.Integration.Utils

runV010IntegrationTests :: FilePath -> IO ()
runV010IntegrationTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/test/Core/Tests/Integration/Version010"
  runIntegrationTests testDirectory [checkColorsFiles, checkFontsFiles, checkAlertsFiles, checkErrorsFiles] []
