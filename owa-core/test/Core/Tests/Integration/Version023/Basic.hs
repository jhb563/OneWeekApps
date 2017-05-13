-- OWALib will expose a method:
-- runOWA :: FilePath -> [String] -> IO ()
-- which will run the One Week Apps program, searching for
-- an app directory, finding relevant files, and producing
-- the correct output files. We will test this on the cases
-- included in Version 0.2.3, primarily Swift file generation.
-- We do not test features like Lazy Code Generation, 
-- Selective Code Type generation, or App Info CLI here,
-- because those tests are already based on runOWA.

module Core.Tests.Integration.Version023.Basic (
  runV023IntegrationTests
) where

import System.Directory (createDirectoryIfMissing)
import Test.Hspec

import Core.Tests.Integration.Utils
import Core.Tests.Utils

runV023IntegrationTests :: FilePath -> IO ()
runV023IntegrationTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/test/Core/Tests/Integration/Version023"
  let outputDirectory = currentDirectory ++ "/test/Core/Tests/Integration/Version023/swift/IntegrationApp/"
  createDirectoryIfMissing True outputDirectory 
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
view1Result = "/swift/IntegrationApp/VIAConstraintTest2.swift"

view1Test :: String
view1Test = "/swift/IntegrationApp/VIAConstraintTest2.swift.test"
