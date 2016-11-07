-- OWALib will expose a method:
-- runOWA :: FilePath -> [String] -> IO ()
-- which will run the One Week Apps program, searching for
-- an app directory, finding relevant files, and producing
-- the correct output files. We will test this on the cases
-- included in Version 0.2.3, primarily Swift file generation.
-- We do not test features like Lazy Code Generation, 
-- Selective Code Type generation, or App Info CLI here,
-- because those tests are already based on runOWA.

module Version023IntegrationTests (
  runV023IntegrationTests
) where

import IntegrationTestUtil
import TestUtil
import Test.Hspec

runV023IntegrationTests :: FilePath -> IO ()
runV023IntegrationTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/tests/Version023Tests/IntegrationTests"
  runIntegrationTestsSwift testDirectory 
    [checkColorsFilesSwift,
    checkFontsFilesSwift,
    checkAlertsFilesSwift,
    checkErrorsFilesSwift,
    checkStringsFiles,
    checkViewsFiles]
    additionalFiles

checkViewsFiles :: FilePath -> Spec
checkViewsFiles testDirectory = do
  let fullView1Result = testDirectory ++ view1Result
  let fullView1Test = testDirectory ++ view1Test
  describe "Compare Produced Swift Views Files" $
    it "The file should match" $
      fullView1Result `filesShouldMatch` fullView1Test

additionalFiles :: [FilePath]
additionalFiles = [view1Result]

view1Result :: String
view1Result = "/app/VIAConstraintTest2.swift"

view1Test :: String
view1Test = "/app/VIAConstraintTest2.swift.test"
