-- Common elements between all of our integration tests, such as
-- extensions for certain produced files, specs for comparing them, etc.

module IntegrationTestUtil (
  runIntegrationTests,
  checkAlertsFiles,
  checkColorsFiles,
  checkErrorsFiles,
  checkFontsFiles,
  checkStringsFiles
) where

import OWALib
import TestUtil
import Test.Hspec

runIntegrationTests :: FilePath -> [(FilePath -> Spec)] -> [String] -> IO ()
runIntegrationTests testDirectory specs additionalFiles = hspec $
  beforeAll_ (removeDiffFiles $ testDirectory ++ appExtension) $
  beforeAll_ (runOWA testDirectory [])
  . afterAll_ (removeProducedFiles testDirectory additionalFiles) $ do
    sequence_ $ map (\specFun -> specFun testDirectory) specs

checkColorsFiles :: FilePath -> Spec
checkColorsFiles testDirectory = do
  let producedColorHeaderFilePath = testDirectory ++ colorHeaderFileExtension
  let producedColorImplementationFilePath = testDirectory ++ colorImplementationFileExtension
  let testColorHeaderFilePath = testDirectory ++ colorHeaderTestExtension
  let testColorImplementationFilePath = testDirectory ++ colorImplementationTestExtension
  describe "Compare Produced Colors Files" $ do
    it "Header File Should Match" $
      producedColorHeaderFilePath `filesShouldMatch` testColorHeaderFilePath

    it "Implementation File Should Match" $
      producedColorImplementationFilePath `filesShouldMatch` testColorImplementationFilePath

checkFontsFiles :: FilePath -> Spec
checkFontsFiles testDirectory = do
  let producedFontHeaderFilePath = testDirectory ++ fontHeaderFileExtension
  let producedFontImplementationFilePath = testDirectory ++ fontImplementationFileExtension
  let testFontHeaderFilePath = testDirectory ++ fontHeaderTestExtension
  let testFontImplementationFilePath = testDirectory ++ fontImplementationTestExtension
  describe "Compare Produced Fonts Files" $ do
    it "Header File Should Match" $
      producedFontHeaderFilePath `filesShouldMatch` testFontHeaderFilePath

    it "Implementation File Should Match" $
      producedFontImplementationFilePath `filesShouldMatch` testFontImplementationFilePath

checkAlertsFiles :: FilePath -> Spec
checkAlertsFiles testDirectory = do
  let producedAlertHeaderFilePath = testDirectory ++ alertHeaderFileExtension
  let producedAlertImplementationFilePath = testDirectory ++ alertImplementationFileExtension
  let testAlertHeaderFilePath = testDirectory ++ alertHeaderTestExtension
  let testAlertImplementationFilePath = testDirectory ++ alertImplementationTestExtension
  describe "Compare Produced Alerts Files" $ do
    it "Header File Should Match" $
      producedAlertHeaderFilePath `filesShouldMatch` testAlertHeaderFilePath

    it "Implementation File Should Match" $
      producedAlertImplementationFilePath `filesShouldMatch` testAlertImplementationFilePath

checkErrorsFiles :: FilePath -> Spec
checkErrorsFiles testDirectory = do
  let producedErrorHeaderFilePath = testDirectory ++ errorHeaderFileExtension
  let producedErrorImplementationFilePath = testDirectory ++ errorImplementationFileExtension
  let testErrorHeaderFilePath = testDirectory ++ errorHeaderTestExtension
  let testErrorImplementationFilePath = testDirectory ++ errorImplementationTestExtension
  describe "Compare Produced Errors Files" $ do
    it "Header File Should Match" $
      producedErrorHeaderFilePath `filesShouldMatch` testErrorHeaderFilePath

    it "Implementation File Should Match" $
      producedErrorImplementationFilePath `filesShouldMatch` testErrorImplementationFilePath

checkStringsFiles :: FilePath -> Spec
checkStringsFiles testDirectory = do
  let producedStringsFilePath = testDirectory ++ localizableStringsFileExtension
  let testStringsFilePath = testDirectory ++ localizableStringsTestExtension
  describe "Compare Produced Strings Files" $ do
    it "The Localizable.strings file should match" $
      producedStringsFilePath `filesShouldMatch` testStringsFilePath

removeProducedFiles :: FilePath -> [String] -> IO ()
removeProducedFiles testDirectory additionalFiles = removeFiles $ map (testDirectory ++) (producedFiles ++ additionalFiles)

appExtension :: String
appExtension = "/app"

colorHeaderFileExtension :: String
colorHeaderFileExtension = "/app/UIColor+IGAColors.h"

colorImplementationFileExtension :: String
colorImplementationFileExtension = "/app/UIColor+IGAColors.m"

colorHeaderTestExtension :: String
colorHeaderTestExtension = "/app/UIColor+IGAColors.h.test"

colorImplementationTestExtension :: String
colorImplementationTestExtension = "/app/UIColor+IGAColors.m.test"

fontHeaderFileExtension :: String
fontHeaderFileExtension = "/app/UIFont+IGAFonts.h"

fontImplementationFileExtension :: String
fontImplementationFileExtension = "/app/UIFont+IGAFonts.m"

fontHeaderTestExtension :: String
fontHeaderTestExtension = "/app/UIFont+IGAFonts.h.test"

fontImplementationTestExtension :: String
fontImplementationTestExtension = "/app/UIFont+IGAFonts.m.test"

alertHeaderFileExtension :: String
alertHeaderFileExtension = "/app/UIAlertController+IGAAlerts.h"

alertImplementationFileExtension :: String
alertImplementationFileExtension = "/app/UIAlertController+IGAAlerts.m"

alertHeaderTestExtension :: String
alertHeaderTestExtension = "/app/UIAlertController+IGAAlerts.h.test"

alertImplementationTestExtension :: String
alertImplementationTestExtension = "/app/UIAlertController+IGAAlerts.m.test"

errorHeaderFileExtension :: String
errorHeaderFileExtension = "/app/NSError+IGAErrors.h"

errorImplementationFileExtension :: String
errorImplementationFileExtension = "/app/NSError+IGAErrors.m"

errorHeaderTestExtension :: String
errorHeaderTestExtension = "/app/NSError+IGAErrors.h.test"

errorImplementationTestExtension :: String
errorImplementationTestExtension = "/app/NSError+IGAErrors.m.test"

localizedStringFileExtension :: String
localizedStringFileExtension = "/app/Localizable.strings"

localizedStringsTestExtension :: String
localizedStringsTestExtension = "/app/Localizable.strings.test"

producedFiles :: [FilePath]
producedFiles = [colorHeaderFileExtension, colorImplementationFileExtension,
  fontHeaderFileExtension, fontImplementationFileExtension,
  alertHeaderFileExtension, alertImplementationFileExtension,
  errorHeaderFileExtension, errorImplementationFileExtension,
  localizedStringFileExtension]
