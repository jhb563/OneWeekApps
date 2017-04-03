-- OWALib will expose a method:
-- runOWA :: FilePath -> [String] -> IO ()
-- which will run the One Week Apps program, searching for
-- an app directory, finding relevant files, and producing
-- the correct output files. We will test this on the cases
-- included in Version 0.3.0, primarily model file generation.

module Core.Tests.Integration.Version030.Basic (
  runV030IntegrationTests
) where

runV030IntegrationTests :: FilePath -> IO ()
runV030IntegrationTests currentDirectory = print "Version 0.3.0 Integration Tests"
