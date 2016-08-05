module LazyCodeGenerationTests (
  runLazyCodeGenerationTests
) where

import Control.Monad.Reader
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
  resultBools <- runTests testDirectory
  hspec $
    afterAll_ (removeProducedFiles testDirectory) $ do
      runATest testDirectory

runTests :: FilePath -> IO [Bool]
runTests testDirectory = do
  runOWA testDirectory ["generate"]
  let trueProducedFiles = map (testDirectory ++) producedFiles
  fileTimeMap <- createFileTimeMap trueProducedFiles
  nanosleep 1000
  runOWA testDirectory ["generate"]
  newFileTimeMap <- createFileTimeMap trueProducedFiles
  let noImmediateChanges = fileTimeMap == newFileTimeMap
  return [noImmediateChanges]

timeChangesAfterModification
  :: FilePath -- The path to run generate on
  -> FilePath -- The file we are modifying
  -> [FilePath] -- The files whose times should change
  -> FileTimeMap -- The map containing the files and times
  -> IO Bool
timeChangesAfterModification testDirectory fileToChange producedFiles fileTimeMap = do
  -- Check that the files current modification times match the map
  currentTime <- getCurrentTime
  setModificationTime fileToChange currentTime
  runOWA testDirectory ["generate"]
  -- Check that the files new modification time does NOT map the map

createFileTimeMap :: [FilePath] -> IO FileTimeMap
createFileTimeMap files = do
  modificationTimes <- mapM getModificationTime files
  return $ Map.fromList (zip files modificationTimes)

runATest :: FilePath -> Spec
runATest currentDirectory = do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      1 `shouldBe` (2 - 1)

    it "throws an exception if used with an empty list" $ do
      2 `shouldBe` (4 - 2)

removeProducedFiles :: FilePath -> IO ()
removeProducedFiles testDirectory = removeFiles $ 
  map (testDirectory ++) producedFiles

appDirectoryExtension :: FilePath
appDirectoryExtension = "/tests/Version023Tests/LazyCodeTests/app"

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
