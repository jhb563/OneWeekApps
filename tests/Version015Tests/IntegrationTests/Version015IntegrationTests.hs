-- OWALib will expose a method:
-- runOWA :: IO ()
-- which will run the One Week Apps program, searching for
-- an app directory, finding relevant files, and producing
-- the correct output files. We will test this on the cases
-- included in Version 0.1.5. These include localized string
-- creation, commenting of files, indentation flexibility,
-- parse failures, and the app.info files.

module Version015IntegrationTests (
  runV015IntegrationTests
) where

import OWALib
import TestUtil
import Test.Hspec

runV015IntegrationTests :: FilePath -> IO ()
runV015IntegrationTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/tests/Version015Tests/IntegrationTests"
  hspec $
  beforeAll_ (removeDiffFiles $ testDirectory ++ appExtension) $
  beforeAll_ (runOWA testDirectory [])
  .afterAll_ (removeProducedFiles testDirectory) $ do
    checkStringsFiles testDirectory

checkStringsFiles :: FilePath -> Spec
checkStringsFiles testDirectory = do
  let producedStringsFilePath = testDirectory ++ localizableStringsFileExtension
  let testStringsFilePath = testDirectory ++ localizableStringsTestExtension
  describe "Compare Produced Strings Files" $ do
    it "The Localizable.strings file should match" $
      producedStringsFilePath `filesShouldMatch` testStringsFilePath

removeProducedFiles :: FilePath -> IO ()
removeProducedFiles testDirectory = removeFiles $ map (testDirectory ++) producedFiles

appExtension :: String
appExtension = "/app"

localizableStringsFileExtension :: String
localizableStringsFileExtension = "/app/Localizable.strings"

localizableStringsTestExtension :: String
localizableStringsTestExtension = "/app/Localizable.strings.test"

producedFiles :: [String]
producedFiles = ["/app/Localizable.strings"]
