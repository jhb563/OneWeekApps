-- OWALib will expose a method:
-- runOWA :: FilePath -> [String] -> IO ()
-- which will run the One Week Apps program, searching for
-- an app directory, finding relevant files, and producing
-- the correct output files. We will test this on the cases
-- included in Version 0.1.5. These include localized string
-- creation, commenting of files, indentation flexibility,
-- parse failures, and the app.info files.

module Core.Tests.Integration.Version015.Basic (
  runV015IntegrationTests
) where

import Core.Tests.Integration.Utils

runV015IntegrationTests :: FilePath -> IO ()
runV015IntegrationTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/test/Core/Tests/Integration/Version015"
  runIntegrationTestsObjc testDirectory
    [checkColorsFilesObjc,
    checkFontsFilesObjc,
    checkAlertsFilesObjc,
    checkErrorsFilesObjc,
    checkStringsFilesObjc] 
    []
