-- This module will test the method:
-- findStringsFiles :: FilePath -> IO [FilePath]
-- exposed by OWAFileSearch. We will pass a directory
-- to this method and receive a list of all .strings files
-- which are found beneath it.

module StringsSearchTests (
  runStringsSearchTests
) where

import OWAFileSearch
import System.Directory
import TestUtil
import Test.Hspec

runStringsSearchTests :: FilePath -> IO ()
runStringsSearchTests currentDirectory = hspec $
  beforeAll_ (setupTestEnv currentDirectory)
  . afterAll_ (tearDownTestEnv currentDirectory) $ do
    findStringsBasicTest currentDirectory
    findStringsMultiLevelTest currentDirectory
    findStringsMultiFileTest currentDirectory
    findStringsFailTest currentDirectory
    findStringsRedHerringTest currentDirectory

findStringsBasicTest :: FilePath -> Spec
findStringsBasicTest currentDirectory = do
  let basicPath = currentDirectory ++ basicTestExtension
  let results = map (currentDirectory ++) basicTestResults
  describe "Find single strings file in the app directory" $ 
    it "Should locate a single file" $ 
      findStringsFiles basicPath `shouldReturn` results

findStringsMultiLevelTest :: FilePath -> Spec
findStringsMultiLevelTest currentDirectory = do
  let multiLevelPath = currentDirectory ++ multiLevelExtension
  let results = map (currentDirectory ++) multiLevelResults
  describe "Find strings files located a multiple levels below the app directory" $ 
    it "Should locate several strings files" $
      findStringsFiles multiLevelPath `shouldReturnSorted` results

findStringsMultiFileTest :: FilePath -> Spec
findStringsMultiFileTest currentDirectory = do
  let multiFilePath = currentDirectory ++ multiFileExtension
  let results = map (currentDirectory ++) multiFileResults
  describe "Find strings files when several are in the same directory" $ 
    it "Should locate several strings files" $
      findStringsFiles multiFilePath `shouldReturnSorted` results

findStringsFailTest :: FilePath -> Spec
findStringsFailTest currentDirectory = do
  let failFilePath = currentDirectory ++ failTestExtension
  describe "Find strings files when there are none" $ 
    it "Should return no strings files" $
      findStringsFiles failFilePath `shouldReturn` []

findStringsRedHerringTest :: FilePath -> Spec
findStringsRedHerringTest currentDirectory = do
  let redHerringFilePath = currentDirectory ++ redHerringTestExtension
  describe "Find strings when there are red herring files" $
    context "when we have a file called Localizable.strings, and files with other extensions" $
      it "Should return no strings files" $
        findStringsFiles redHerringFilePath `shouldReturn` []

setupTestEnv :: FilePath -> IO ()
setupTestEnv currentDirectory = do
  mapM_ (\fp -> createDirectoryIfMissing True $ currentDirectory ++ fp) directoryExtensions
  mapM_ (createFileAndClose currentDirectory) fileExtensions

tearDownTestEnv :: FilePath -> IO ()
tearDownTestEnv currentDirectory = removeDirectoryRecursive (currentDirectory ++ testEnvExtension)

basicTestResults :: [FilePath]
basicTestResults = [basicTestExtension ++ "/mystrings.strings"]

multiLevelResults :: [FilePath]
multiLevelResults = map (multiLevelExtension ++)
  ["/dir1/dir11/strings2.strings",
  "/dir1/strings1.strings",
  "/dir2/strings3.strings",
  "/dir3/strings4.strings",
  "/strings0.strings"]

multiFileResults :: [FilePath]
multiFileResults = map (multiFileExtension ++)
  ["/stringsFolder/strings1.strings",
  "/stringsFolder/strings2.strings",
  "/stringsFolder/strings3.strings"]

testEnvExtension :: FilePath
testEnvExtension = "/testenv"

basicTestExtension :: FilePath
basicTestExtension = "/testenv/app1"

multiLevelExtension :: FilePath
multiLevelExtension = "/testenv/app2"

multiFileExtension :: FilePath
multiFileExtension = "/testenv/app3"

failTestExtension :: FilePath
failTestExtension = "/testenv/app4"

redHerringTestExtension :: FilePath
redHerringTestExtension = "/testenv/app5"

directoryExtensions :: [FilePath]
directoryExtensions = [basicTestExtension,
  multiLevelExtension ++ "/dir1/dir11",
  multiLevelExtension ++ "/dir2",
  multiLevelExtension ++ "/dir3",
  multiFileExtension ++ "/stringsFolder",
  failTestExtension ++ "/dir1/dir11/dir111",
  failTestExtension ++ "/dir1/dir12",
  failTestExtension ++ "/dir2",
  failTestExtension ++ "/dir3",
  redHerringTestExtension,
  redHerringTestExtension ++ "/dir1/strings"]

fileExtensions :: [FilePath]
fileExtensions = concat [basicTestResults,
  multiLevelResults,
  multiFileResults,
  redHerringFiles]

redHerringFiles :: [FilePath]
redHerringFiles = map (redHerringTestExtension ++)
  ["/Localizable.strings",
  "/mycolors.colors",
  "/myfonts.fonts",
  "/myalerts.alerts",
  "/myerrors.errors",
  "/strings.xml",
  "/strings.json",
  "/strings"]

-- Setup Directory Structure
-- | testenv
-- -- | app1
-- -- -- mystrings.strings
-- -- | app2
-- -- -- | strings0.strings
-- -- -- | dir1
-- -- -- -- | strings1.strings
-- -- -- -- | dir11
-- -- -- -- -- | strings2.strings
-- -- -- | dir2
-- -- -- -- | strings3.strings
-- -- -- | dir3
-- -- -- -- | strings4.strings
-- -- | app3
-- -- -- | stringsFolder
-- -- -- -- | strings1.strings
-- -- -- -- | strings2.strings
-- -- -- -- | strings3.strings
-- -- | app4
-- -- -- | dir1
-- -- -- -- | dir11
-- -- -- -- -- | dir111
-- -- -- -- | dir12
-- -- -- | dir2
-- -- -- | dir3
-- -- | app5
-- -- -- | Localizable.strings
-- -- -- | mycolors.colors
-- -- -- | myfonts.fonts
-- -- -- | myalerts.alerts
-- -- -- | myerrors.errors
-- -- -- | strings.xml
-- -- -- | strings.json
-- -- -- | strings (no extension)
-- -- -- | dir1
-- -- -- -- | strings (directory)
