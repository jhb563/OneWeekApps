-- OWALib will expose a method:
-- runOWA :: IO ()
-- which will run the One Week Apps program, searching for
-- an app directory, finding relevant files, and producing
-- the correct output files. 
module Version010IntegrationTests (
  runV010IntegrationTests
) where

import IntegrationTestUtil

runV010IntegrationTests :: FilePath -> IO ()
runV010IntegrationTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/tests/Version010Tests/IntegrationTests"
  runIntegrationTests testDirectory [checkColorsFiles, checkFontsFiles, checkAlertsFiles, checkErrorsFiles] []

