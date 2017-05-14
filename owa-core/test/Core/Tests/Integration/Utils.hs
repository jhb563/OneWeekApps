-- Common elements between all of our integration tests, such as
-- extensions for certain produced files, specs for comparing them, etc.

module Core.Tests.Integration.Utils (
  runIntegrationTestsObjc,
  runIntegrationTestsSwift,
  checkAlertsFilesObjc,
  checkColorsFilesObjc,
  checkErrorsFilesObjc,
  checkFontsFilesObjc,
  checkAlertsFilesSwift,
  checkColorsFilesSwift,
  checkErrorsFilesSwift,
  checkFontsFilesSwift,
  checkStringsFilesObjc,
  checkStringsFiles
) where

import System.IO
import Test.Hspec

import Core.Main
import Core.Tests.Utils

runIntegrationTestsObjc :: FilePath -> [FilePath -> Spec] -> [String] -> IO ()
runIntegrationTestsObjc testDirectory specs additionalFiles = hspec $
  beforeAll_ (removeDiffFiles $ testDirectory ++ projectExtensionObjc) $
  beforeAll_ (runOWA stdin stdout testDirectory ["generate"])
  . afterAll_ (removeProducedFiles testDirectory additionalFiles) $
    mapM_ (\specFun -> specFun testDirectory) specs

runIntegrationTestsSwift :: FilePath -> [FilePath -> Spec] -> [String] -> IO ()
runIntegrationTestsSwift testDirectory specs additionalFiles = hspec $
  beforeAll_ (removeDiffFiles $ testDirectory ++ projectExtension) $
  beforeAll_ (runOWA stdin stdout testDirectory ["generate", "--swift"])
  . afterAll_ (removeProducedFilesSwift testDirectory additionalFiles) $
    mapM_ (\specFun -> specFun testDirectory) specs

checkColorsFilesObjc :: FilePath -> Spec
checkColorsFilesObjc testDirectory = do
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

checkFontsFilesObjc :: FilePath -> Spec
checkFontsFilesObjc testDirectory = do
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

checkAlertsFilesObjc :: FilePath -> Spec
checkAlertsFilesObjc testDirectory = do
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

checkErrorsFilesObjc :: FilePath -> Spec
checkErrorsFilesObjc testDirectory = do
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

checkStringsFilesObjc :: FilePath -> Spec
checkStringsFilesObjc testDirectory = do
  let producedStringsFilePath = testDirectory ++ localizedStringFileObjcExtension
  let testStringsFilePath = testDirectory ++ localizedStringsObjcTestExtension
  describe "Compare Produced Strings Files" $
    it "The Localizable.strings file should match" $
      producedStringsFilePath `filesShouldMatch` testStringsFilePath

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
projectExtension = "/swift/IntegrationApp"

projectExtensionObjc :: String
projectExtensionObjc = "/objc/IntegrationApp"

colorHeaderFileExtension :: String
colorHeaderFileExtension = "/objc/IntegrationApp/UIColor+IGAColors.h"

colorImplementationFileExtension :: String
colorImplementationFileExtension = "/objc/IntegrationApp/UIColor+IGAColors.m"

colorHeaderTestExtension :: String
colorHeaderTestExtension = "/objc/IntegrationApp/UIColor+IGAColors.h.test"

colorImplementationTestExtension :: String
colorImplementationTestExtension = "/objc/IntegrationApp/UIColor+IGAColors.m.test"

colorSwiftResult :: String
colorSwiftResult = "/swift/IntegrationApp/UIColor+IGAColors.swift"

colorSwiftTest :: String
colorSwiftTest = "/swift/IntegrationApp/UIColor+IGAColors.swift.test"

fontHeaderFileExtension :: String
fontHeaderFileExtension = "/objc/IntegrationApp/UIFont+IGAFonts.h"

fontImplementationFileExtension :: String
fontImplementationFileExtension = "/objc/IntegrationApp/UIFont+IGAFonts.m"

fontHeaderTestExtension :: String
fontHeaderTestExtension = "/objc/IntegrationApp/UIFont+IGAFonts.h.test"

fontImplementationTestExtension :: String
fontImplementationTestExtension = "/objc/IntegrationApp/UIFont+IGAFonts.m.test"

fontSwiftResult :: String
fontSwiftResult = "/swift/IntegrationApp/UIFont+IGAFonts.swift"

fontSwiftTest :: String
fontSwiftTest = "/swift/IntegrationApp/UIFont+IGAFonts.swift.test"

alertHeaderFileExtension :: String
alertHeaderFileExtension = "/objc/IntegrationApp/UIAlertController+IGAAlerts.h"

alertImplementationFileExtension :: String
alertImplementationFileExtension = "/objc/IntegrationApp/UIAlertController+IGAAlerts.m"

alertHeaderTestExtension :: String
alertHeaderTestExtension = "/objc/IntegrationApp/UIAlertController+IGAAlerts.h.test"

alertImplementationTestExtension :: String
alertImplementationTestExtension = "/objc/IntegrationApp/UIAlertController+IGAAlerts.m.test"

alertSwiftResult :: String
alertSwiftResult = "/swift/IntegrationApp/UIAlertController+IGAAlerts.swift"

alertSwiftTest :: String
alertSwiftTest = "/swift/IntegrationApp/UIAlertController+IGAAlerts.swift.test"

errorHeaderFileExtension :: String
errorHeaderFileExtension = "/objc/IntegrationApp/NSError+IGAErrors.h"

errorImplementationFileExtension :: String
errorImplementationFileExtension = "/objc/IntegrationApp/NSError+IGAErrors.m"

errorHeaderTestExtension :: String
errorHeaderTestExtension = "/objc/IntegrationApp/NSError+IGAErrors.h.test"

errorImplementationTestExtension :: String
errorImplementationTestExtension = "/objc/IntegrationApp/NSError+IGAErrors.m.test"

errorSwiftResult :: String
errorSwiftResult = "/swift/IntegrationApp/NSError+IGAErrors.swift"

errorSwiftTest :: String
errorSwiftTest = "/swift/IntegrationApp/NSError+IGAErrors.swift.test"

localizedStringFileObjcExtension :: String
localizedStringFileObjcExtension = "/objc/IntegrationApp/Localizable.strings"

localizedStringsObjcTestExtension :: String
localizedStringsObjcTestExtension = "/objc/IntegrationApp/Localizable.strings.test"

localizedStringFileExtension :: String
localizedStringFileExtension = "/swift/IntegrationApp/Localizable.strings"

localizedStringsTestExtension :: String
localizedStringsTestExtension = "/swift/IntegrationApp/Localizable.strings.test"

lastGenFileExtension :: String
lastGenFileExtension = "/app/.owa_last_gen"

producedFiles :: [FilePath]
producedFiles = [lastGenFileExtension,
  colorHeaderFileExtension, colorImplementationFileExtension,
  fontHeaderFileExtension, fontImplementationFileExtension,
  alertHeaderFileExtension, alertImplementationFileExtension,
  errorHeaderFileExtension, errorImplementationFileExtension,
  localizedStringFileObjcExtension]

swiftProducedFiles :: [FilePath]
swiftProducedFiles = [lastGenFileExtension,
  colorSwiftResult, fontSwiftResult, 
  alertSwiftResult, errorSwiftResult,
  localizedStringFileExtension ]
