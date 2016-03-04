-- OWALib will expose a method:
-- runOWA :: IO ()
-- which will run the One Week Apps program, searching for
-- an app directory, finding relevant files, and producing
-- the correct output files. 
module Version010IntegrationTests (
  runV010IntegrationTests
) where

import OWALib
import TestUtil
import Test.Hspec

runV010IntegrationTests :: FilePath -> IO ()
runV010IntegrationTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/tests/IntegrationTests"
  hspec $
    beforeAll_ (removeDiffFiles $ testDirectory ++ appExtension) $
    beforeAll_ (runOWA testDirectory)
    . afterAll_ (removeProducedFiles testDirectory) $ do
      checkColorsFiles testDirectory
      checkFontsFiles testDirectory

checkColorsFiles :: FilePath -> Spec
checkColorsFiles currentDirectory = do
  let producedColorHeaderFilePath = currentDirectory ++ colorHeaderFileExtension
  let producedColorImplementationFilePath = currentDirectory ++ colorImplementationFileExtension
  let testColorHeaderFilePath = currentDirectory ++ colorHeaderTestExtension
  let testColorImplementationFilePath = currentDirectory ++ colorImplementationTestExtension
  describe "Compare Produced Colors File" $ do
    it "Header File Should Match" $
      producedColorHeaderFilePath `filesShouldMatch` testColorHeaderFilePath

    it "Implementation File Should Match" $
      producedColorImplementationFilePath `filesShouldMatch` testColorImplementationFilePath

checkFontsFiles :: FilePath -> Spec
checkFontsFiles currentDirectory = do
  let producedFontHeaderFilePath = currentDirectory ++ fontHeaderFileExtension
  let producedFontImplementationFilePath = currentDirectory ++ fontImplementationFileExtension
  let testFontHeaderFilePath = currentDirectory ++ fontHeaderTestExtension
  let testFontImplementationFilePath = currentDirectory ++ fontImplementationTestExtension
  describe "Compare Produced Fonts File" $ do
    it "Header File Should Match" $
      producedFontHeaderFilePath `filesShouldMatch` testFontHeaderFilePath

    it "Implementation File Should Match" $
      producedFontImplementationFilePath `filesShouldMatch` testFontImplementationFilePath

removeProducedFiles :: FilePath -> IO ()
removeProducedFiles currentDirectory = removeFiles $ map (currentDirectory ++) producedFiles

appExtension :: String
appExtension = "/app"

colorHeaderFileExtension :: String
colorHeaderFileExtension = "/app/UIColor+MyAppColors.h"

colorImplementationFileExtension :: String
colorImplementationFileExtension = "/app/UIColor+MyAppColors.m"

colorHeaderTestExtension :: String
colorHeaderTestExtension = "/app/UIColor+MyAppColors.h.test"

colorImplementationTestExtension :: String
colorImplementationTestExtension = "/app/UIColor+MyAppColors.m.test"

fontHeaderFileExtension :: String
fontHeaderFileExtension = "/app/UIFont+MyAppFonts.h"

fontImplementationFileExtension :: String
fontImplementationFileExtension = "/app/UIFont+MyAppFonts.m"

fontHeaderTestExtension :: String
fontHeaderTestExtension = "/app/UIFont+MyAppFonts.h.test"

fontImplementationTestExtension :: String
fontImplementationTestExtension = "/app/UIFont+MyAppFonts.m.test"

producedFiles :: [FilePath]
producedFiles = [colorHeaderFileExtension, colorImplementationFileExtension,
  fontHeaderFileExtension, fontImplementationFileExtension]
