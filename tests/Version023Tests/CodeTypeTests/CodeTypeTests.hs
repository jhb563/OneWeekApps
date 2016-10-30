-- This module will test runOWA with different combinations
-- of arguments, testing specifically that certain types
-- of code are either generated or not generated.

module CodeTypeTests (
  runCodeTypeTests
) where

import Control.Monad (forM)
import OWALib (runOWA)
import System.Directory (doesFileExist)
import System.IO (stdin, stdout)
import Test.Hspec
import TestUtil

data FileTest = FileExists FilePath | FileDoesNotExist FilePath 
  deriving (Show, Eq)

runCodeTypeTests :: FilePath -> IO ()
runCodeTypeTests currentDirectory = do
  let testDirectory = currentDirectory ++ appDirectoryExtension
  hspec $ after_ (removeResultsFiles testDirectory allProducedFiles) $ do
    colorsTest testDirectory
    fontsTest testDirectory
    alertsTest testDirectory
    errorsTest testDirectory
    viewsTest testDirectory
    stringsTest testDirectory
    colorsPlusViewsTest testDirectory
    alertsPlusStringsTest testDirectory
    noArgsTest testDirectory

colorsTest :: FilePath -> Spec
colorsTest testDirectory = describe "When only generating colors files" $ 
  before_ (runOWA stdin stdout testDirectory ["generate", "--colors"]) $
    it "Should only generate colors files" $
      checkExactlyFilesExist testDirectory [producedColorHeader, producedColorM]

fontsTest :: FilePath -> Spec
fontsTest testDirectory = describe "When only generating fonts files" $ 
  before_ (runOWA stdin stdout testDirectory ["generate", "--fonts"]) $
    it "Should only generate fonts files" $
      checkExactlyFilesExist testDirectory [producedFontHeader, producedFontM]

alertsTest :: FilePath -> Spec
alertsTest testDirectory = describe "When only generating alerts files" $ 
  before_ (runOWA stdin stdout testDirectory ["generate", "--alerts"]) $
    it "Should only generate alerts files" $
      checkExactlyFilesExist testDirectory [producedAlertHeader, producedAlertM]

errorsTest :: FilePath -> Spec
errorsTest testDirectory = describe "When only generating errors files" $ 
  before_ (runOWA stdin stdout testDirectory ["generate", "--errors"]) $
    it "Should only generate errors files" $
      checkExactlyFilesExist testDirectory [producedErrorHeader, producedErrorM]

viewsTest :: FilePath -> Spec
viewsTest testDirectory = describe "When only generating views files" $ 
  before_ (runOWA stdin stdout testDirectory ["generate", "--views"]) $
    it "Should only generate views files" $
      checkExactlyFilesExist testDirectory 
        [producedView1Header, producedView1M, producedView2Header, producedView2M]

stringsTest :: FilePath -> Spec
stringsTest testDirectory = describe "When only generating strings files" $ 
  before_ (runOWA stdin stdout testDirectory ["generate", "--strings"]) $
    it "Should only generate strings files" $
      checkExactlyFilesExist testDirectory [producedStrings]

colorsPlusViewsTest :: FilePath -> Spec
colorsPlusViewsTest testDirectory = describe "When generating colors and views" $ 
  before_ (runOWA stdin stdout testDirectory ["generate", "--views", "--colors"]) $
    it "Should generate views and colors files" $
      checkExactlyFilesExist testDirectory 
        [producedView1Header, producedView1M
        , producedView2Header, producedView2M
        , producedColorHeader, producedColorM ]

alertsPlusStringsTest :: FilePath -> Spec
alertsPlusStringsTest testDirectory = describe "When generating alerts and strings" $ 
  before_ (runOWA stdin stdout testDirectory ["generate", "--strings", "--alerts"]) $
    it "Should generate strings and alerts files" $
      checkExactlyFilesExist testDirectory 
        [producedStrings, producedAlertHeader, producedAlertM ]

noArgsTest :: FilePath -> Spec
noArgsTest testDirectory = describe "When generating with no arguments" $
  before_ (runOWA stdin stdout testDirectory ["generate"]) $
    it "Should generate all files" $
      checkExactlyFilesExist testDirectory allPossibleProducedFiles

checkExactlyFilesExist :: FilePath -> [FilePath] -> Expectation
checkExactlyFilesExist testDirectory expectedFiles = do
  let filesToExist = map (testDirectory ++) expectedFiles
  let filesToNotExist = map (testDirectory ++) (filter notExpected allPossibleProducedFiles)
  _ <- forM filesToExist (\file -> do
    test <- fileTest file
    test `shouldBe` FileExists file)
  _ <- forM filesToNotExist (\file -> do
    test <- fileTest file
    test `shouldBe` FileDoesNotExist file)
  return ()
  where
    notExpected filePath = filePath `notElem` expectedFiles

fileTest :: FilePath -> IO FileTest
fileTest file = do
  exists <- doesFileExist file
  if exists
    then return $ FileExists file
    else return $ FileDoesNotExist file

appDirectoryExtension :: FilePath
appDirectoryExtension = "/tests/Version023Tests/CodeTypeTests/app"

allProducedFiles :: [FilePath]
allProducedFiles = lastGenPath : allPossibleProducedFiles

allPossibleProducedFiles :: [FilePath]
allPossibleProducedFiles =
  [ producedColorHeader, producedColorM
  , producedFontHeader, producedFontM
  , producedAlertHeader, producedAlertM
  , producedErrorHeader, producedErrorM
  , producedView1Header, producedView1M
  , producedView2Header, producedView2M
  , producedStrings ]

lastGenPath :: FilePath
lastGenPath = "/.owa_last_gen"

producedColorHeader :: FilePath
producedColorHeader = "/UIColor+IGAColors.h"

producedColorM :: FilePath
producedColorM = "/UIColor+IGAColors.m"

producedFontHeader :: FilePath
producedFontHeader = "/UIFont+IGAFonts.h"

producedFontM :: FilePath
producedFontM = "/UIFont+IGAFonts.m"

producedAlertHeader :: FilePath
producedAlertHeader = "/UIAlertController+IGAAlerts.h"

producedAlertM :: FilePath
producedAlertM = "/UIAlertController+IGAAlerts.m"

producedErrorHeader :: FilePath
producedErrorHeader = "/NSError+IGAErrors.h"

producedErrorM :: FilePath
producedErrorM = "/NSError+IGAErrors.m"

producedView1Header :: FilePath
producedView1Header = "/IGAView1.h"

producedView1M :: FilePath
producedView1M = "/IGAView1.m"

producedView2Header :: FilePath
producedView2Header = "/IGAView2.h"

producedView2M :: FilePath
producedView2M = "/IGAView2.m"

producedStrings :: FilePath
producedStrings = "/Localizable.strings"
