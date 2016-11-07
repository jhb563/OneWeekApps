-- Common elements between all of our integration tests, such as
-- extensions for certain produced files, specs for comparing them, etc.

module IntegrationTestUtil (
  runIntegrationTests,
  runIntegrationTestsSwift,
  checkAlertsFiles,
  checkColorsFiles,
  checkErrorsFiles,
  checkFontsFiles,
  checkAlertsFilesSwift,
  checkColorsFilesSwift,
  checkErrorsFilesSwift,
  checkFontsFilesSwift,
  checkStringsFiles
) where

import OWALib
import System.IO
import TestUtil
import Test.Hspec

runIntegrationTests :: FilePath -> [FilePath -> Spec] -> [String] -> IO ()
runIntegrationTests testDirectory specs additionalFiles = hspec $
  beforeAll_ (removeDiffFiles $ testDirectory ++ appExtension) $
  beforeAll_ (runOWA stdin stdout testDirectory ["generate"])
  . afterAll_ (removeProducedFiles testDirectory additionalFiles) $
    mapM_ (\specFun -> specFun testDirectory) specs

runIntegrationTestsSwift :: FilePath -> [FilePath -> Spec] -> [String] -> IO ()
runIntegrationTestsSwift testDirectory specs additionalFiles = hspec $
  beforeAll_ (removeDiffFiles $ testDirectory ++ appExtension) $
  beforeAll_ (runOWA stdin stdout testDirectory ["generate", "--swift"])
  . afterAll_ (removeProducedFilesSwift testDirectory additionalFiles) $
    mapM_ (\specFun -> specFun testDirectory) specs

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

checkColorsFilesSwift :: FilePath -> Spec
checkColorsFilesSwift testDirectory = do
  let producedColorFilePath = testDirectory ++ colorSwiftResult
  let testColorFilePath = testDirectory ++ colorSwiftTest
  describe "Compare Produced Swift Colors Files" $
    it "The file should match" $
      producedColorFilePath `filesShouldMatch` testColorFilePath

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

checkFontsFilesSwift :: FilePath -> Spec
checkFontsFilesSwift testDirectory = do
  let producedFontFilePath = testDirectory ++ fontSwiftResult
  let testFontFilePath = testDirectory ++ fontSwiftTest
  describe "Compare Produced Swift Fonts Files" $
    it "The file should match" $
      producedFontFilePath `filesShouldMatch` testFontFilePath

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

checkAlertsFilesSwift :: FilePath -> Spec
checkAlertsFilesSwift testDirectory = do
  let producedAlertFilePath = testDirectory ++ alertSwiftResult
  let testAlertFilePath = testDirectory ++ alertSwiftTest
  describe "Compare Produced Swift Alerts Files" $
    it "The file should match" $
      producedAlertFilePath `filesShouldMatch` testAlertFilePath

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

checkErrorsFilesSwift :: FilePath -> Spec
checkErrorsFilesSwift testDirectory = do
  let producedErrorFilePath = testDirectory ++ errorSwiftResult
  let testErrorFilePath = testDirectory ++ errorSwiftTest
  describe "Compare Produced Swift Errors Files" $
    it "The file should match" $
      producedErrorFilePath `filesShouldMatch` testErrorFilePath

checkStringsFiles :: FilePath -> Spec
checkStringsFiles testDirectory = do
  let producedStringsFilePath = testDirectory ++ localizedStringFileExtension
  let testStringsFilePath = testDirectory ++ localizedStringsTestExtension
  describe "Compare Produced Strings Files" $
    it "The Localizable.strings file should match" $
      producedStringsFilePath `filesShouldMatch` testStringsFilePath

removeProducedFiles :: FilePath -> [String] -> IO ()
removeProducedFiles testDirectory additionalFiles = removeFiles $ map (testDirectory ++) (producedFiles ++ additionalFiles)

removeProducedFilesSwift :: FilePath -> [String] -> IO ()
removeProducedFilesSwift testDirectory additionalFiles = removeFiles $ map (testDirectory ++) (swiftProducedFiles ++ additionalFiles)

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

colorSwiftResult :: String
colorSwiftResult = "/app/UIColor+MSAColors.swift"

colorSwiftTest :: String
colorSwiftTest = "/app/UIColor+MSAColors.swift.test"

fontHeaderFileExtension :: String
fontHeaderFileExtension = "/app/UIFont+IGAFonts.h"

fontImplementationFileExtension :: String
fontImplementationFileExtension = "/app/UIFont+IGAFonts.m"

fontHeaderTestExtension :: String
fontHeaderTestExtension = "/app/UIFont+IGAFonts.h.test"

fontImplementationTestExtension :: String
fontImplementationTestExtension = "/app/UIFont+IGAFonts.m.test"

fontSwiftResult :: String
fontSwiftResult = "/app/UIFont+MSAFonts.swift"

fontSwiftTest :: String
fontSwiftTest = "/app/UIFont+MSAFonts.swift.test"

alertHeaderFileExtension :: String
alertHeaderFileExtension = "/app/UIAlertController+IGAAlerts.h"

alertImplementationFileExtension :: String
alertImplementationFileExtension = "/app/UIAlertController+IGAAlerts.m"

alertHeaderTestExtension :: String
alertHeaderTestExtension = "/app/UIAlertController+IGAAlerts.h.test"

alertImplementationTestExtension :: String
alertImplementationTestExtension = "/app/UIAlertController+IGAAlerts.m.test"

alertSwiftResult :: String
alertSwiftResult = "/app/UIAlertController+MSAAlerts.swift"

alertSwiftTest :: String
alertSwiftTest = "/app/UIAlertController+MSAAlerts.swift.test"

errorHeaderFileExtension :: String
errorHeaderFileExtension = "/app/NSError+IGAErrors.h"

errorImplementationFileExtension :: String
errorImplementationFileExtension = "/app/NSError+IGAErrors.m"

errorHeaderTestExtension :: String
errorHeaderTestExtension = "/app/NSError+IGAErrors.h.test"

errorImplementationTestExtension :: String
errorImplementationTestExtension = "/app/NSError+IGAErrors.m.test"

errorSwiftResult :: String
errorSwiftResult = "/app/NSError+MSAErrors.swift"

errorSwiftTest :: String
errorSwiftTest = "/app/NSError+MSAErrors.swift.test"

localizedStringFileExtension :: String
localizedStringFileExtension = "/app/Localizable.strings"

localizedStringsTestExtension :: String
localizedStringsTestExtension = "/app/Localizable.strings.test"

lastGenFileExtension :: String
lastGenFileExtension = "/app/.owa_last_gen"

producedFiles :: [FilePath]
producedFiles = [lastGenFileExtension,
  colorHeaderFileExtension, colorImplementationFileExtension,
  fontHeaderFileExtension, fontImplementationFileExtension,
  alertHeaderFileExtension, alertImplementationFileExtension,
  errorHeaderFileExtension, errorImplementationFileExtension,
  localizedStringFileExtension]

swiftProducedFiles :: [FilePath]
swiftProducedFiles = [lastGenFileExtension,
  colorSwiftResult, fontSwiftResult, 
  alertSwiftResult, errorSwiftResult,
  localizedStringFileExtension ]
