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

import OWAMain
import System.IO
import TestUtil
import Test.Hspec

runIntegrationTests :: FilePath -> [FilePath -> Spec] -> [String] -> IO ()
runIntegrationTests testDirectory specs additionalFiles = hspec $
  beforeAll_ (removeDiffFiles $ testDirectory ++ projectExtension) $
  beforeAll_ (runOWA stdin stdout testDirectory ["generate"])
  . afterAll_ (removeProducedFiles testDirectory additionalFiles) $
    mapM_ (\specFun -> specFun testDirectory) specs

runIntegrationTestsSwift :: FilePath -> [FilePath -> Spec] -> [String] -> IO ()
runIntegrationTestsSwift testDirectory specs additionalFiles = hspec $
  beforeAll_ (removeDiffFiles $ testDirectory ++ projectExtension) $
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

projectExtension :: String
projectExtension = "/ios/IntegrationApp"

colorHeaderFileExtension :: String
colorHeaderFileExtension = "/ios/IntegrationApp/UIColor+IGAColors.h"

colorImplementationFileExtension :: String
colorImplementationFileExtension = "/ios/IntegrationApp/UIColor+IGAColors.m"

colorHeaderTestExtension :: String
colorHeaderTestExtension = "/ios/IntegrationApp/UIColor+IGAColors.h.test"

colorImplementationTestExtension :: String
colorImplementationTestExtension = "/ios/IntegrationApp/UIColor+IGAColors.m.test"

colorSwiftResult :: String
colorSwiftResult = "/ios/IntegrationApp/UIColor+MSAColors.swift"

colorSwiftTest :: String
colorSwiftTest = "/ios/IntegrationApp/UIColor+MSAColors.swift.test"

fontHeaderFileExtension :: String
fontHeaderFileExtension = "/ios/IntegrationApp/UIFont+IGAFonts.h"

fontImplementationFileExtension :: String
fontImplementationFileExtension = "/ios/IntegrationApp/UIFont+IGAFonts.m"

fontHeaderTestExtension :: String
fontHeaderTestExtension = "/ios/IntegrationApp/UIFont+IGAFonts.h.test"

fontImplementationTestExtension :: String
fontImplementationTestExtension = "/ios/IntegrationApp/UIFont+IGAFonts.m.test"

fontSwiftResult :: String
fontSwiftResult = "/ios/IntegrationApp/UIFont+MSAFonts.swift"

fontSwiftTest :: String
fontSwiftTest = "/ios/IntegrationApp/UIFont+MSAFonts.swift.test"

alertHeaderFileExtension :: String
alertHeaderFileExtension = "/ios/IntegrationApp/UIAlertController+IGAAlerts.h"

alertImplementationFileExtension :: String
alertImplementationFileExtension = "/ios/IntegrationApp/UIAlertController+IGAAlerts.m"

alertHeaderTestExtension :: String
alertHeaderTestExtension = "/ios/IntegrationApp/UIAlertController+IGAAlerts.h.test"

alertImplementationTestExtension :: String
alertImplementationTestExtension = "/ios/IntegrationApp/UIAlertController+IGAAlerts.m.test"

alertSwiftResult :: String
alertSwiftResult = "/ios/IntegrationApp/UIAlertController+MSAAlerts.swift"

alertSwiftTest :: String
alertSwiftTest = "/ios/IntegrationApp/UIAlertController+MSAAlerts.swift.test"

errorHeaderFileExtension :: String
errorHeaderFileExtension = "/ios/IntegrationApp/NSError+IGAErrors.h"

errorImplementationFileExtension :: String
errorImplementationFileExtension = "/ios/IntegrationApp/NSError+IGAErrors.m"

errorHeaderTestExtension :: String
errorHeaderTestExtension = "/ios/IntegrationApp/NSError+IGAErrors.h.test"

errorImplementationTestExtension :: String
errorImplementationTestExtension = "/ios/IntegrationApp/NSError+IGAErrors.m.test"

errorSwiftResult :: String
errorSwiftResult = "/ios/IntegrationApp/NSError+MSAErrors.swift"

errorSwiftTest :: String
errorSwiftTest = "/ios/IntegrationApp/NSError+MSAErrors.swift.test"

localizedStringFileExtension :: String
localizedStringFileExtension = "/ios/IntegrationApp/Localizable.strings"

localizedStringsTestExtension :: String
localizedStringsTestExtension = "/ios/IntegrationApp/Localizable.strings.test"

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
