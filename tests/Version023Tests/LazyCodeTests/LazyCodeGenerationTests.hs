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

import qualified Data.Map as Map
import Data.Time.Clock
import OWALib
import System.Directory
import System.Posix.Unistd
import TestUtil
import Test.Hspec

-- Maps from produced files to their creation times
type FileTimeMap = Map.Map FilePath UTCTime

runLazyCodeGenerationTests :: FilePath -> IO ()
runLazyCodeGenerationTests currentDirectory = do
  let testDirectory = currentDirectory ++ appDirectoryExtension
  resultBools <- createTestResults testDirectory
  let [noImmediateChanges, allChangedAfterAppInfo,
        alertsUnchanged, alertsChangedCorrectly,
        colorsUnchanged, colorsChangedCorrectly,
        errorsUnchanged, errorsChangedCorrectly,
        fontsUnchanged, fontsChangedCorrectly,
        stringsUnchanged, stringsChangedCorrectly,
        firstViewUnchanged, firstViewChangedCorrectly,
        secondViewUnchanged, secondViewChangedCorrectly,
        thirdViewUnchanged, thirdViewChangedCorrectly] = resultBools
  hspec $
    afterAll_ (removeProducedFiles testDirectory) $ do
      runImmediateChangeTest noImmediateChanges
      runAppInfoChangeTest allChangedAfterAppInfo
      runAlertChangeTest alertsUnchanged alertsChangedCorrectly
      runColorChangeTest colorsUnchanged colorsChangedCorrectly
      runErrorChangeTest errorsUnchanged errorsChangedCorrectly
      runFontChangeTest fontsUnchanged fontsChangedCorrectly
      runStringsChangeTest stringsUnchanged stringsChangedCorrectly
      runFirstViewChangeTest firstViewUnchanged firstViewChangedCorrectly
      runSecondViewChangeTest secondViewUnchanged secondViewChangedCorrectly
      runThirdViewChangeTest thirdViewUnchanged thirdViewChangedCorrectly

runImmediateChangeTest :: Bool -> Spec
runImmediateChangeTest noImmediateChanges =
  describe "Test for new code generation after repeat generate" $
    it "Should not have made any changes to the produced files" $
      noImmediateChanges `shouldBe` True

runAppInfoChangeTest :: Bool -> Spec
runAppInfoChangeTest changedAfterAppInfo =
  describe "Test that all files changed after updating the app info" $
    it "Should have changed all the files" $
      changedAfterAppInfo `shouldBe` True

runAlertChangeTest :: Bool -> Bool -> Spec
runAlertChangeTest unchangedBefore changedAfterModification =
  describe "Test for Alerts changing after modifying .alerts file" $ do
    it "Should not have changed before the modification" $
      unchangedBefore `shouldBe` True

    it "Should have changed after modifying .alerts file" $
      changedAfterModification `shouldBe` True

runColorChangeTest :: Bool -> Bool -> Spec
runColorChangeTest unchangedBefore changedAfterModification =
  describe "Test for Colors changing after modifying .colors file" $ do
    it "Should not have changed before the modification" $
      unchangedBefore `shouldBe` True

    it "Should have changed after modifying .colors file" $
      changedAfterModification `shouldBe` True

runErrorChangeTest :: Bool -> Bool -> Spec
runErrorChangeTest unchangedBefore changedAfterModification =
  describe "Test for Errors changing after modifying .errors file" $ do
    it "Should not have changed before the modification" $
      unchangedBefore `shouldBe` True

    it "Should have changed after modifying .errors file" $
      changedAfterModification `shouldBe` True

runFontChangeTest :: Bool -> Bool -> Spec
runFontChangeTest unchangedBefore changedAfterModification =
  describe "Test for Fonts changing after modifying .fonts file" $ do
    it "Should not have changed before the modification" $
      unchangedBefore `shouldBe` True

    it "Should have changed after modifying .fonts file" $
      changedAfterModification `shouldBe` True

runStringsChangeTest :: Bool -> Bool -> Spec
runStringsChangeTest unchangedBefore changedAfterModification =
  describe "Test for Strings changing after modifying .strings file" $ do
    it "Should not have changed before the modification" $
      unchangedBefore `shouldBe` True

    it "Should have changed after modifying .strings file" $
      changedAfterModification `shouldBe` True

runFirstViewChangeTest :: Bool -> Bool -> Spec
runFirstViewChangeTest unchangedBefore changedAfterModification =
  describe "Test for First View changing after modifying .view file" $ do
    it "Should not have changed before the modification" $
      unchangedBefore `shouldBe` True

    it "Should have changed after modifying .view file" $
      changedAfterModification `shouldBe` True

runSecondViewChangeTest :: Bool -> Bool -> Spec
runSecondViewChangeTest unchangedBefore changedAfterModification =
  describe "Test for Second View changing after modifying .view file" $ do
    it "Should not have changed before the modification" $
      unchangedBefore `shouldBe` True

    it "Should have changed after modifying .view file" $
      changedAfterModification `shouldBe` True

runThirdViewChangeTest :: Bool -> Bool -> Spec
runThirdViewChangeTest unchangedBefore changedAfterModification =
  describe "Test for Third View changing after modifying .view file" $ do
    it "Should not have changed before the modification" $
      unchangedBefore `shouldBe` True

    it "Should have changed after modifying .view file" $
      changedAfterModification `shouldBe` True

createTestResults :: FilePath -> IO [Bool]
createTestResults testDirectory = do
  runOWA testDirectory ["generate"]
  let trueProducedFiles = map (testDirectory ++) producedFiles
  fileTimeMap <- createFileTimeMap trueProducedFiles
  nanosleep 1000000000
  runOWA testDirectory ["generate"]
  newFileTimeMap <- createFileTimeMap trueProducedFiles
  let noImmediateChanges = fileTimeMap == newFileTimeMap
  nanosleep 1000000000
  currentTime <- getCurrentTime
  setModificationTime (testDirectory ++ appInfoFile) currentTime 
  runOWA testDirectory ["generate"]
  newestFileTimeMap <- createFileTimeMap trueProducedFiles
  let allChangedAfterAppInfo = checkAllChanged fileTimeMap newestFileTimeMap
  let modifiedPairs = map (modifyPair testDirectory) changePairs
  modificationChanges <- mapM (timeChangesAfterModification testDirectory newestFileTimeMap) modifiedPairs
  return $ noImmediateChanges : allChangedAfterAppInfo : concat modificationChanges

checkAllChanged :: FileTimeMap -> FileTimeMap -> Bool
checkAllChanged oldMap newMap = all checkDifferent (Map.keys oldMap)
  where checkDifferent fp = case (Map.lookup fp oldMap, Map.lookup fp newMap) of
                              (Just t1, Just t2) -> t1 /= t2
                              _ -> False

timeChangesAfterModification
  :: FilePath -- The path to run generate on
  -> FileTimeMap -- The map containing the files and times
  -> (FilePath, [FilePath]) -- The change pair
  -> IO [Bool]
timeChangesAfterModification testDirectory fileTimeMap (fileToChange, producedFiles) = do
  initialResults <- mapM (fileTimeMatches fileTimeMap) producedFiles
  nanosleep 1000000000
  currentTime <- getCurrentTime
  setModificationTime fileToChange currentTime
  runOWA testDirectory ["generate"]
  finalResults <- mapM (fileTimeMatches fileTimeMap) producedFiles
  return [and initialResults, not $ or finalResults]

fileTimeMatches :: FileTimeMap -> FilePath -> IO Bool
fileTimeMatches map_ file = do
  trueModTime <- getModificationTime file
  case Map.lookup file map_ of
    Nothing -> return False
    Just time -> return $ trueModTime == time

createFileTimeMap :: [FilePath] -> IO FileTimeMap
createFileTimeMap files = do
  modificationTimes <- mapM getModificationTime files
  return $ Map.fromList (zip files modificationTimes)

removeProducedFiles :: FilePath -> IO ()
removeProducedFiles testDirectory = removeFiles $ 
  map (testDirectory ++) (lastGenFile : producedFiles)

appDirectoryExtension :: FilePath
appDirectoryExtension = "/tests/Version023Tests/LazyCodeTests/app"

lastGenFile :: FilePath
lastGenFile = "/.owa_last_gen"

producedFiles :: [FilePath]
producedFiles = [producedColorHeader, producedColorM,
  producedFontHeader, producedFontM,
  producedAlertHeader, producedAlertM,
  producedErrorHeader, producedErrorM,
  producedStrings,
  producedView1Header, producedView1M,
  producedView2Header, producedView2M,
  producedView3Header, producedView3M]

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

changePairs :: [(FilePath, [FilePath])]
changePairs = [("/viaalerts.alerts", [producedAlertHeader, producedAlertM]),
  ("/viacolors.colors", [producedColorHeader, producedColorM]),
  ("/viaerrors.errors", [producedErrorHeader, producedErrorM]),
  ("/viafonts.fonts", [producedFontHeader, producedFontM]),
  ("/views/VIAFirstView.view", [producedView1Header, producedView1M]),
  ("/views/VIASecondView.view", [producedView2Header, producedView2M]),
  ("/views/thirdview.view", [producedView3Header, producedView3M]),
  ("/viastrings.strings", [producedStrings])]

modifyPair :: FilePath -> (FilePath, [FilePath]) -> (FilePath, [FilePath])
modifyPair base (srcFile, prodFiles) = (base ++ srcFile, map (base ++) prodFiles)
