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
    testCorrectFilesChange testDirectory colorsPair "Colors regenerated properly"
    testCorrectFilesChange testDirectory fontsPair "Fonts regenerated properly"
    testCorrectFilesChange testDirectory alertsPair "Alerts regenerated properly"
    testCorrectFilesChange testDirectory errorsPair "Errors regenerated properly"
    testCorrectFilesChange testDirectory view1Pair "View 1 regenerated properly"
    testCorrectFilesChange testDirectory view2Pair "View 2 regenerated properly"
    testCorrectFilesChange testDirectory view3Pair "View 3 regenerated properly"
    testCorrectFilesChange testDirectory stringsPair "Strings regenerated properly"

noChangePair :: ([FilePath], [FilePath])
noChangePair = ([], [])

appInfoPair :: ([FilePath], [FilePath])
appInfoPair = ([appInfoFile], producedFiles)

colorsPair :: ([FilePath], [FilePath])
colorsPair = ([colorsInput1], [producedColorHeader, producedColorM])

fontsPair :: ([FilePath], [FilePath])
fontsPair = ([fontsInput1], [producedFontHeader, producedFontM])

alertsPair :: ([FilePath], [FilePath])
alertsPair = ([alertsInput1], [producedAlertHeader, producedAlertM])

errorsPair :: ([FilePath], [FilePath])
errorsPair = ([errorsInput1], [producedErrorHeader, producedErrorM])

view1Pair :: ([FilePath], [FilePath])
view1Pair = ([viewsInput1], [producedView1Header, producedView1M])

view2Pair :: ([FilePath], [FilePath])
view2Pair = ([viewsInput2], [producedView2Header, producedView2M])

view3Pair :: ([FilePath], [FilePath])
view3Pair = ([viewsInput3], [producedView3Header, producedView3M])

stringsPair :: ([FilePath], [FilePath])
stringsPair = ([stringsInput1], [producedStrings])

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
  mapM_ ((flip setModificationTime) earlierTime) (map (testDirectory ++) (allInputFiles ++ allFiles))

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

allInputFiles :: [FilePath]
allInputFiles = 
  [ appInfoFile
  , colorsInput1
  , fontsInput1
  , alertsInput1
  , errorsInput1
  , viewsInput1
  , viewsInput2
  , viewsInput3 
  , stringsInput1 ]

lastGenFile :: FilePath
lastGenFile = "/.owa_last_gen"

colorsInput1 :: FilePath
colorsInput1 = "/viacolors.colors"

producedColorHeader :: FilePath
producedColorHeader = "/UIColor+IGAColors.h"

producedColorM :: FilePath
producedColorM  = "/UIColor+IGAColors.m"

fontsInput1 :: FilePath
fontsInput1 = "/viafonts.fonts"

producedFontHeader :: FilePath
producedFontHeader = "/UIFont+IGAFonts.h"

producedFontM :: FilePath
producedFontM  = "/UIFont+IGAFonts.m"

alertsInput1 :: FilePath
alertsInput1 = "/viaalerts.alerts"

producedAlertHeader :: FilePath
producedAlertHeader = "/UIAlertController+IGAAlerts.h"

producedAlertM :: FilePath
producedAlertM  = "/UIAlertController+IGAAlerts.m"

errorsInput1 :: FilePath
errorsInput1 = "/viaerrors.errors"

producedErrorHeader :: FilePath
producedErrorHeader = "/NSError+IGAErrors.h"

producedErrorM :: FilePath
producedErrorM  = "/NSError+IGAErrors.m"

stringsInput1 :: FilePath
stringsInput1 = "/viastrings.strings"

producedStrings :: FilePath
producedStrings = "/Localizable.strings"

producedView1Header :: FilePath
producedView1Header = "/VIAFirstView.h"

viewsInput1 :: FilePath
viewsInput1 = "/views/VIAFirstView.view"

producedView1M :: FilePath
producedView1M = "/VIAFirstView.m"

viewsInput2 :: FilePath
viewsInput2 = "/views/VIASecondView.view"

producedView2Header :: FilePath
producedView2Header = "/VIASecondView.h"

producedView2M :: FilePath
producedView2M = "/VIASecondView.m"

viewsInput3 :: FilePath
viewsInput3 = "/views/thirdview.view"

producedView3Header :: FilePath
producedView3Header = "/VIAThirdView.h"

producedView3M :: FilePath
producedView3M = "/VIAThirdView.m"

appInfoFile :: FilePath
appInfoFile = "/app.info"
