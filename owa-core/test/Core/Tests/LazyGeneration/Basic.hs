-- This module will test runOWA, specifically testing
-- in different circumstances of whether or not new
-- Objective C code should actually be generated.
--
-- Note that due to the complexity of wrapping Spec
-- in an IO AND reading from a common source, the
-- test values are all created before hand in an IO,
-- and then run through hspec.

module Core.Tests.LazyGeneration.Basic (
  runLazyCodeGenerationTests
) where

import Data.Time.Clock
import System.Directory
import System.IO (stdin, stdout)
import Test.Hspec

import Core.Main
import Core.Tests.Utils

runLazyCodeGenerationTests :: FilePath -> IO ()
runLazyCodeGenerationTests currentDirectory = do
  let testDirectory = currentDirectory ++ appDirectoryExtension
  let outputDirectory = currentDirectory ++ outputDirectoryExtension
  let dirTupleSwift = (testDirectory, outputDirectory)
  let outputDirectoryObjc = currentDirectory ++ outputDirectoryExtensionObjc
  let dirTupleObjc = (testDirectory, outputDirectoryObjc)
  hspec $ do
    testCorrectFilesChangeObjc dirTupleObjc noChangePair "If no files changed, no regeneration"
    testCorrectFilesChangeObjc dirTupleObjc appInfoPair "If app info changes, all files regenerate"
    testCorrectFilesChangeObjc dirTupleObjc colorsPair "Colors regenerated properly"
    testCorrectFilesChangeObjc dirTupleObjc fontsPair "Fonts regenerated properly"
    testCorrectFilesChangeObjc dirTupleObjc alertsPair "Alerts regenerated properly"
    testCorrectFilesChangeObjc dirTupleObjc errorsPair "Errors regenerated properly"
    testCorrectFilesChangeObjc dirTupleObjc view1Pair "View 1 regenerated properly"
    testCorrectFilesChangeObjc dirTupleObjc view2Pair "View 2 regenerated properly"
    testCorrectFilesChangeObjc dirTupleObjc view3Pair "View 3 regenerated properly"
    testCorrectFilesChangeObjc dirTupleObjc model1Pair "Model 1 regenerated properly"
    testCorrectFilesChangeObjc dirTupleObjc model2Pair "Model 2 regenerated properly"
    testCorrectFilesChangeObjc dirTupleObjc stringsPair "Strings regenerated properly"
    testCorrectFilesChangeObjc dirTupleObjc colorFontsPair "Colors and fonts regenerated together"
    testCorrectFilesChangeObjc dirTupleObjc alertStringsViewPair 
      "Alerts, strings, and a view regenerated together"
    testCorrectFilesChangeSwift dirTupleSwift noChangePairSwift "(swift) If no files changed, no regeneration"
    testCorrectFilesChangeSwift dirTupleSwift appInfoPairSwift "(swift) If app info changes, all files regenerate"
    testCorrectFilesChangeSwift dirTupleSwift colorsPairSwift "(swift) Colors regenerated properly"
    testCorrectFilesChangeSwift dirTupleSwift fontsPairSwift "(swift) Fonts regenerated properly"
    testCorrectFilesChangeSwift dirTupleSwift alertsPairSwift "(swift) Alerts regenerated properly"
    testCorrectFilesChangeSwift dirTupleSwift errorsPairSwift "(swift) Errors regenerated properly"
    testCorrectFilesChangeSwift dirTupleSwift view1PairSwift "(swift) View 1 regenerated properly"
    testCorrectFilesChangeSwift dirTupleSwift view2PairSwift "(swift) View 2 regenerated properly"
    testCorrectFilesChangeSwift dirTupleSwift view3PairSwift "(swift) View 3 regenerated properly"
    testCorrectFilesChangeSwift dirTupleSwift model1PairSwift "(swift) Model 1 regenerated properly"
    testCorrectFilesChangeSwift dirTupleSwift model2PairSwift "(swift) Model 2 regenerated properly"
    testCorrectFilesChangeSwift dirTupleSwift stringsPairSwift "(swift) Strings regenerated properly"
    testCorrectFilesChangeSwift dirTupleSwift colorFontsPairSwift "(swift) Colors and fonts regenerated together"
    testCorrectFilesChangeSwift dirTupleSwift alertStringsViewPairSwift 
      "(swift) Alerts, strings, and a view regenerated together"
    testMultiLanguageFilesChange
      (beforeHookWithArgs ["generate"] ["generate", "--swift"] producedFiles)
      (producedFiles ++ producedFilesSwift)
      (testDirectory, outputDirectoryObjc, outputDirectory)
      objcBeforeSwiftPair
      "Swift files should be generated even after Objc"
    testMultiLanguageFilesChange
      (beforeHookWithArgs ["generate", "--swift"] ["generate"] producedFilesSwift)
      (producedFiles ++ producedFilesSwift)
      (testDirectory, outputDirectory, outputDirectoryObjc)
      swiftBeforeObjcPair
      "Objc files should be generated even after Swift"

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

model1Pair :: ([FilePath], [FilePath])
model1Pair = ([modelsInput1], [producedModel1Header, producedModel1M])

model2Pair :: ([FilePath], [FilePath])
model2Pair = ([modelsInput2], [producedModel2Header, producedModel2M])

stringsPair :: ([FilePath], [FilePath])
stringsPair = ([stringsInput1], [producedStrings])

colorFontsPair :: ([FilePath], [FilePath])
colorFontsPair = ([colorsInput2, fontsInput2], 
  [producedColorHeader, producedColorM, producedFontHeader, producedFontM])

alertStringsViewPair :: ([FilePath], [FilePath])
alertStringsViewPair = ([alertsInput1, stringsInput1, viewsInput2],
  [ producedAlertHeader, producedAlertM
  , producedStrings
  , producedView2Header, producedView2M])

noChangePairSwift :: ([FilePath], [FilePath])
noChangePairSwift = ([], [])

appInfoPairSwift :: ([FilePath], [FilePath])
appInfoPairSwift = ([appInfoFile], producedFilesSwift)

colorsPairSwift :: ([FilePath], [FilePath])
colorsPairSwift = ([colorsInput1], [producedColorSwift])

fontsPairSwift :: ([FilePath], [FilePath])
fontsPairSwift = ([fontsInput1], [producedFontSwift])

alertsPairSwift :: ([FilePath], [FilePath])
alertsPairSwift = ([alertsInput1], [producedAlertSwift])

errorsPairSwift :: ([FilePath], [FilePath])
errorsPairSwift = ([errorsInput1], [producedErrorSwift])

view1PairSwift :: ([FilePath], [FilePath])
view1PairSwift = ([viewsInput1], [producedView1Swift])

view2PairSwift :: ([FilePath], [FilePath])
view2PairSwift = ([viewsInput2], [producedView2Swift])

view3PairSwift :: ([FilePath], [FilePath])
view3PairSwift = ([viewsInput3], [producedView3Swift])

model1PairSwift :: ([FilePath], [FilePath])
model1PairSwift = ([modelsInput1], [producedModel1Swift])

model2PairSwift :: ([FilePath], [FilePath])
model2PairSwift = ([modelsInput2], [producedModel2Swift])

stringsPairSwift :: ([FilePath], [FilePath])
stringsPairSwift = ([stringsInput1], [producedStrings])

colorFontsPairSwift :: ([FilePath], [FilePath])
colorFontsPairSwift = ([colorsInput2, fontsInput2], 
  [producedColorSwift, producedFontSwift])

alertStringsViewPairSwift :: ([FilePath], [FilePath])
alertStringsViewPairSwift = ([alertsInput1, stringsInput1, viewsInput2],
  [ producedAlertSwift
  , producedStrings
  , producedView2Swift, producedView2Swift])

swiftBeforeObjcPair :: ([FilePath], [FilePath])
swiftBeforeObjcPair = ([], producedFiles)

objcBeforeSwiftPair :: ([FilePath], [FilePath])
objcBeforeSwiftPair = ([], producedFilesSwift)

beforeTestHook :: (FilePath, FilePath) -> [FilePath] -> IO ()
beforeTestHook = beforeHookWithArgs ["generate"] ["generate"] producedFiles

swiftBeforeTestHook :: (FilePath, FilePath) -> [FilePath] -> IO ()
swiftBeforeTestHook = beforeHookWithArgs ["generate", "--swift"] ["generate", "--swift"] producedFilesSwift

beforeHookWithArgs :: [String] -> [String] -> [FilePath] -> (FilePath, FilePath) -> [FilePath] -> IO ()
beforeHookWithArgs args1 args2 outputFiles (appDirectory, outputDirectory) inputFiles = do
  runOWA stdin stdout appDirectory args1
  setModificationTimesBack (appDirectory, outputDirectory) outputFiles timePrefix
  modifyInputFiles (map (appDirectory ++) inputFiles)
  runOWA stdin stdout appDirectory args2
  where
    timePrefix = if "--swift" `elem` args1 then "Swift: " else "Objc: "

-- Take each produced file and set its modification time 5 seconds in the past, so that we
-- can see the immediate results of another run of runOWA without waiting a full second.
-- (This is necessary because many File Systems only store file files to a full second resolution.
setModificationTimesBack :: (FilePath, FilePath) -> [FilePath] -> String -> IO ()
setModificationTimesBack (appDirectory, outputDirectory) files timePrefix = do
  earlierTime <- addUTCTime (-5) <$> getCurrentTime  
  mapM_ ((`setModificationTime` earlierTime) . (appDirectory ++)) allInputFiles
  mapM_ ((`setModificationTime` earlierTime) . (outputDirectory ++)) files
  let timeString = timePrefix ++ show earlierTime ++ "\n"
  writeFile (appDirectory ++ lastGenFile) timeString

modifyInputFiles ::  [FilePath] -> IO ()
modifyInputFiles files = do
  currentTime <- getCurrentTime
  mapM_ (`setModificationTime` currentTime) files
    
-- Takes the Input Files which we're going to modify, then the files which
-- we expect to have been modified.
testCorrectFilesChangeObjc :: (FilePath, FilePath) -> ([FilePath], [FilePath]) -> String -> Spec
testCorrectFilesChangeObjc = testCorrectFilesChange beforeTestHook producedFiles

testCorrectFilesChangeSwift :: (FilePath, FilePath) -> ([FilePath], [FilePath]) -> String -> Spec
testCorrectFilesChangeSwift = testCorrectFilesChange swiftBeforeTestHook producedFilesSwift

testCorrectFilesObjcBeforeSwift :: (FilePath, FilePath) -> ([FilePath], [FilePath]) -> String -> Spec
testCorrectFilesObjcBeforeSwift = testCorrectFilesChange hook (producedFiles ++ producedFilesSwift)
  where
    hook = beforeHookWithArgs ["generate"] ["generate", "--swift"] producedFiles

testCorrectFilesSwiftBeforeObjc :: (FilePath, FilePath) -> ([FilePath], [FilePath]) -> String -> Spec
testCorrectFilesSwiftBeforeObjc = testCorrectFilesChange hook (producedFiles ++ producedFilesSwift)
  where
    hook = beforeHookWithArgs ["generate", "--swift"] ["generate"] producedFilesSwift

testCorrectFilesChange :: ((FilePath, FilePath) -> [FilePath] -> IO ()) -> [FilePath] ->
  (FilePath, FilePath) -> ([FilePath], [FilePath]) -> String -> Spec
testCorrectFilesChange hook prodFiles (appDirectory, outputDirectory) (inputFiles, outputFiles) description = beforeAll_ (hook (appDirectory, outputDirectory) inputFiles)
  . afterAll_ (removeResultsFiles outputDirectory prodFiles) $
    afterAll_ (removeResultsFiles appDirectory [lastGenFile]) $
    describe description $ do
      let expectedRegeneratedFiles = map (outputDirectory ++) outputFiles
      let staleOutputFiles = filter (`notElem` outputFiles) prodFiles
      let expectedUnregeneratedFiles = map (outputDirectory ++) staleOutputFiles
      it "Certain files should be regenerated" $
        mapM_ shouldBeRegenerated expectedRegeneratedFiles
      it "Certain files should NOT be regenerated" $
        mapM_ shouldNotBeRegenerated expectedUnregeneratedFiles

testMultiLanguageFilesChange :: ((FilePath, FilePath) -> [FilePath] -> IO ()) -> [FilePath] ->
  (FilePath, FilePath, FilePath) -> ([FilePath], [FilePath]) -> String -> Spec
testMultiLanguageFilesChange 
  hook prodFiles (appDirectory, outputDir1, outputDir2) (inputFiles, outputFiles) description =
    beforeAll_ (hook (appDirectory, outputDir1) inputFiles)
      . afterAll_ (removeResultsFiles outputDir1 prodFiles) $
        afterAll_ (removeResultsFiles outputDir2 prodFiles) $
        afterAll_ (removeResultsFiles appDirectory [lastGenFile]) $
        describe description $ do
          let expectedRegeneratedFiles = map (outputDir2 ++) outputFiles
          let staleOutputFiles = filter (`notElem` outputFiles) prodFiles
          let expectedUnregeneratedFiles = map (outputDir1 ++) staleOutputFiles
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
  if diffUTCTime currentTime modificationTime < 3
    then FileWasRegenerated file `shouldBe` regenTest 
    else FileWasNotRegenerated file `shouldBe` regenTest

appDirectoryExtension :: FilePath
appDirectoryExtension = "/test/Core/Tests/LazyGeneration/app"

outputDirectoryExtension :: FilePath
outputDirectoryExtension = "/test/Core/Tests/LazyGeneration/swift/ViewIntegrationApp"

outputDirectoryExtensionObjc :: FilePath
outputDirectoryExtensionObjc = "/test/Core/Tests/LazyGeneration/objc/ViewIntegrationApp"

producedFiles :: [FilePath]
producedFiles = [producedColorHeader, producedColorM,
  producedFontHeader, producedFontM,
  producedAlertHeader, producedAlertM,
  producedErrorHeader, producedErrorM,
  producedStrings,
  producedView1Header, producedView1M,
  producedView2Header, producedView2M,
  producedView3Header, producedView3M,
  producedModel1Header, producedModel1M,
  producedModel2Header, producedModel2M]

producedFilesSwift :: [FilePath]
producedFilesSwift = 
  [ producedColorSwift
  , producedFontSwift
  , producedAlertSwift
  , producedErrorSwift
  , producedStrings
  , producedView1Swift
  , producedView2Swift
  , producedView3Swift 
  , producedModel1Swift
  , producedModel2Swift]

allInputFiles :: [FilePath]
allInputFiles = 
  [ appInfoFile
  , colorsInput1
  , colorsInput2
  , fontsInput1
  , fontsInput2
  , alertsInput1
  , errorsInput1
  , viewsInput1
  , viewsInput2
  , viewsInput3 
  , stringsInput1 
  , modelsInput1
  , modelsInput2
  ]

lastGenFile :: FilePath
lastGenFile = "/.owa_last_gen"

colorsInput1 :: FilePath
colorsInput1 = "/viacolors.colors"

colorsInput2 :: FilePath
colorsInput2 = "/viacolors2.colors"

producedColorHeader :: FilePath
producedColorHeader = "/UIColor+IGAColors.h"

producedColorM :: FilePath
producedColorM  = "/UIColor+IGAColors.m"

producedColorSwift :: FilePath
producedColorSwift  = "/UIColor+IGAColors.swift"

fontsInput1 :: FilePath
fontsInput1 = "/viafonts.fonts"

fontsInput2 :: FilePath
fontsInput2 = "/viafonts2.fonts"

producedFontHeader :: FilePath
producedFontHeader = "/UIFont+IGAFonts.h"

producedFontM :: FilePath
producedFontM  = "/UIFont+IGAFonts.m"

producedFontSwift :: FilePath
producedFontSwift  = "/UIFont+IGAFonts.swift"

alertsInput1 :: FilePath
alertsInput1 = "/viaalerts.alerts"

producedAlertHeader :: FilePath
producedAlertHeader = "/UIAlertController+IGAAlerts.h"

producedAlertM :: FilePath
producedAlertM  = "/UIAlertController+IGAAlerts.m"

producedAlertSwift :: FilePath
producedAlertSwift  = "/UIAlertController+IGAAlerts.swift"

errorsInput1 :: FilePath
errorsInput1 = "/viaerrors.errors"

producedErrorHeader :: FilePath
producedErrorHeader = "/NSError+IGAErrors.h"

producedErrorM :: FilePath
producedErrorM  = "/NSError+IGAErrors.m"

producedErrorSwift :: FilePath
producedErrorSwift  = "/NSError+IGAErrors.swift"

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

producedView1Swift :: FilePath
producedView1Swift = "/VIAFirstView.swift"

viewsInput2 :: FilePath
viewsInput2 = "/views/VIASecondView.view"

producedView2Header :: FilePath
producedView2Header = "/VIASecondView.h"

producedView2M :: FilePath
producedView2M = "/VIASecondView.m"

producedView2Swift :: FilePath
producedView2Swift = "/VIASecondView.swift"

viewsInput3 :: FilePath
viewsInput3 = "/views/thirdview.view"

producedView3Header :: FilePath
producedView3Header = "/VIAThirdView.h"

producedView3M :: FilePath
producedView3M = "/VIAThirdView.m"

producedView3Swift :: FilePath
producedView3Swift = "/VIAThirdView.swift"

modelsInput1 :: FilePath
modelsInput1 = "/viamodel1.model"

producedModel1Header :: FilePath
producedModel1Header = "/VIAModel1.h"

producedModel1M :: FilePath
producedModel1M = "/VIAModel1.m"

producedModel1Swift :: FilePath
producedModel1Swift = "/VIAModel1.swift"

modelsInput2 :: FilePath
modelsInput2 = "/viamodel2.model"

producedModel2Header :: FilePath
producedModel2Header = "/VIAModel2.h"

producedModel2M :: FilePath
producedModel2M = "/VIAModel2.m"

producedModel2Swift :: FilePath
producedModel2Swift = "/VIAModel2.swift"

appInfoFile :: FilePath
appInfoFile = "/app.info"
