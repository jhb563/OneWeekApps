-- OWALib will expose a method:
-- runOWA :: IO ()
-- which will run the One Week Apps program, searching for
-- an app directory, finding relevant files, and producing
-- the correct output files. 
module Version010IntegrationTests (
  runV010IntegrationTests
) where

runV010IntegrationTests :: FilePath -> IO ()
runV010IntegrationTests filePath = print "Running Integration Tests"

-- After running the program, we should find that the produced colors
-- files match our test files.