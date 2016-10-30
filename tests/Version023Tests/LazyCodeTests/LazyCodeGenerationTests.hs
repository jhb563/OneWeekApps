-- This module will test runOWA, specifically testing
-- in different circumstances of whether or not new
-- Objective C code should actually be generated.
--
-- Note that due to the complexity of wrapping Spec
-- in an IO AND reading from a common source, the
-- test values are all created before hand in an IO,
-- and then run through hspec.

module LazyCodeGenerationTests (
  runLazyCodeGenerationTests
) where

import Data.Time.Clock
import OWALib
import System.Directory
import TestUtil
import Test.Hspec

runLazyCodeGenerationTests :: FilePath -> IO ()
runLazyCodeGenerationTests currentDirectory = do
  let testDirectory = currentDirectory ++ appDirectoryExtension
  hspec $ do
    testCorrectFilesChange testDirectory noChangePair "If no files changed, no regeneration"
    testCorrectFilesChange testDirectory appInfoPair "If app info changes, all files regenerate"

noChangePair :: ([FilePath], [FilePath])
noChangePair = ([], [])

appInfoPair :: ([FilePath], [FilePath])
appInfoPair = ([appInfoFile], producedFiles)

beforeTestHook :: FilePath -> [FilePath] -> IO ()
beforeTestHook testDirectory inputFiles = do
  runOWA testDirectory ["generate"]
  setModificationTimesBack testDirectory
  modifyInputFiles (map (testDirectory ++) inputFiles)
  runOWA testDirectory ["generate"]

-- Take each produced file and set its modification time 5 seconds in the past, so that we
-- can see the immediate results of another run of runOWA without waiting a full second.
-- (This is necessary because many File Systems only store file files to a full second resolution.
setModificationTimesBack :: FilePath -> IO ()
setModificationTimesBack testDirectory = do
  earlierTime <- addUTCTime (-5) <$> getCurrentTime  
  mapM_ ((flip setModificationTime) earlierTime) (map (testDirectory ++) allFiles)

modifyInputFiles ::  [FilePath] -> IO ()
modifyInputFiles files = do
  currentTime <- getCurrentTime
  mapM_ ((flip setModificationTime) currentTime) files
    
-- Takes the Input Files which we're going to modify, then the files which
-- we expect to have been modified.
testCorrectFilesChange :: FilePath -> ([FilePath], [FilePath]) -> String -> Spec
testCorrectFilesChange testDirectory (inputFiles, outputFiles) description = beforeAll_ (beforeTestHook testDirectory inputFiles)
  . afterAll_ (removeResultsFiles testDirectory allFiles) $ do
    describe description $ do
      let expectedRegeneratedFiles = map (testDirectory ++) outputFiles
      let staleOutputFiles = filter (\file -> not (file `elem` outputFiles)) producedFiles 
      let expectedUnregeneratedFiles = map (testDirectory ++) staleOutputFiles
      it "Certain files should be regenerated" $
        mapM_ shouldBeRegenerated expectedRegeneratedFiles
      it "Certain files should NOT be regenerated" $
        mapM_ shouldNotBeRegenerated expectedUnregeneratedFiles

shouldBeRegenerated :: FilePath -> Expectation
shouldBeRegenerated file = expectRegenerated (FileWasRegenerated file) file

shouldNotBeRegenerated :: FilePath -> Expectation
shouldNotBeRegenerated file = expectRegenerated (FileWasNotRegenerated file) file

data RegenTest = FileWasRegenerated FilePath | FileWasNotRegenerated FilePath
  deriving (Show, Eq)

expectRegenerated :: RegenTest -> FilePath -> Expectation
expectRegenerated regenTest file = do
  currentTime <- getCurrentTime
  modificationTime <- getModificationTime file
  if (diffUTCTime currentTime modificationTime) < 3
    then FileWasRegenerated file `shouldBe` regenTest 
    else FileWasNotRegenerated file `shouldBe` regenTest

appDirectoryExtension :: FilePath
appDirectoryExtension = "/tests/Version023Tests/LazyCodeTests/app"

allFiles :: [FilePath]
allFiles = lastGenFile : producedFiles

producedFiles :: [FilePath]
producedFiles = [producedColorHeader, producedColorM,
  producedFontHeader, producedFontM,
  producedAlertHeader, producedAlertM,
  producedErrorHeader, producedErrorM,
  producedStrings,
  producedView1Header, producedView1M,
  producedView2Header, producedView2M,
  producedView3Header, producedView3M]

lastGenFile :: FilePath
lastGenFile = "/.owa_last_gen"

producedColorHeader :: FilePath
producedColorHeader = "/UIColor+IGAColors.h"

producedColorM :: FilePath
producedColorM  = "/UIColor+IGAColors.m"

producedFontHeader :: FilePath
producedFontHeader = "/UIFont+IGAFonts.h"

producedFontM :: FilePath
producedFontM  = "/UIFont+IGAFonts.m"

producedAlertHeader :: FilePath
producedAlertHeader = "/UIAlertController+IGAAlerts.h"

producedAlertM :: FilePath
producedAlertM  = "/UIAlertController+IGAAlerts.m"

producedErrorHeader :: FilePath
producedErrorHeader = "/NSError+IGAErrors.h"

producedErrorM :: FilePath
producedErrorM  = "/NSError+IGAErrors.m"

producedStrings :: FilePath
producedStrings = "/Localizable.strings"

producedView1Header :: FilePath
producedView1Header = "/VIAFirstView.h"

producedView1M :: FilePath
producedView1M = "/VIAFirstView.m"

producedView2Header :: FilePath
producedView2Header = "/VIASecondView.h"

producedView2M :: FilePath
producedView2M = "/VIASecondView.m"

producedView3Header :: FilePath
producedView3Header = "/VIAThirdView.h"

producedView3M :: FilePath
producedView3M = "/VIAThirdView.m"

appInfoFile :: FilePath
appInfoFile = "/app.info"
