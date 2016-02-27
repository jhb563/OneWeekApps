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
    beforeAll_ (runOWA testDirectory)
    . afterAll_ (removeProducedFiles testDirectory) $ do
      checkColorsFiles testDirectory

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

removeProducedFiles :: FilePath -> IO ()
removeProducedFiles currentDirectory = do
  removeFiles $ map (currentDirectory ++) producedFiles

colorHeaderFileExtension :: String
colorHeaderFileExtension = "/app/UIColor+MyAppColors.h"

colorImplementationFileExtension :: String
colorImplementationFileExtension = "/app/UIColor+MyAppColors.m"

colorHeaderTestExtension :: String
colorHeaderTestExtension = "/app/UIColor+MyAppColors.h.test"

colorImplementationTestExtension :: String
colorImplementationTestExtension = "/app/UIColor+MyAppColors.m.test"

producedFiles :: [FilePath]
producedFiles = [colorHeaderFileExtension, colorImplementationFileExtension]


