-- This module will test the function:
-- findViewsFiles :: FilePath -> IO [FilePath]
-- from OWAFileSearch, which takes a directory and returns
-- a list of paths to .view files.

module ViewFileSearchTests (
  runViewFileSearchTests
) where

import Test.Hspec
import System.Directory

import OWAFileSearch
import TestUtil

runViewFileSearchTests :: FilePath -> IO ()
runViewFileSearchTests currentDirectory = hspec $
  beforeAll_ (setupTestEnv currentDirectory)
  . afterAll_ (tearDownTestEnv currentDirectory) $ do
    findViewsBasicTest currentDirectory
    findViewsMultiLevelTest currentDirectory
    findViewsMultiFileTest currentDirectory
    findViewsFailTest currentDirectory
    findViewsRedHerringTest currentDirectory

findViewsBasicTest :: FilePath -> Spec
findViewsBasicTest currentDirectory = do
  let basicPath = currentDirectory ++ basicTestExtension
  let results = map (currentDirectory ++) basicTestResults
  describe "Find single view file in the app directory" $ 
    it "Should locate a single file" $ 
      findViewsFiles basicPath `shouldReturn` results

findViewsMultiLevelTest :: FilePath -> Spec
findViewsMultiLevelTest currentDirectory = do
  let multiLevelPath = currentDirectory ++ multiLevelExtension
  let results = map (currentDirectory ++) multiLevelResults
  describe "Find views files located multiple levels below the app directory" $ 
    it "Should locate several views files" $
      findViewsFiles multiLevelPath `shouldReturnSorted` results

findViewsMultiFileTest :: FilePath -> Spec
findViewsMultiFileTest currentDirectory = do
  let multiFilePath = currentDirectory ++ multiFileExtension
  let results = map (currentDirectory ++) multiFileResults
  describe "Find views files when several are in the same directory" $ 
    it "Should locate several views files" $
      findViewsFiles multiFilePath `shouldReturnSorted` results

findViewsFailTest :: FilePath -> Spec
findViewsFailTest currentDirectory = do
  let failFilePath = currentDirectory ++ failTestExtension
  describe "Find views files when there are none" $ 
    it "Should return no views files" $
      findViewsFiles failFilePath `shouldReturn` []

findViewsRedHerringTest :: FilePath -> Spec
findViewsRedHerringTest currentDirectory = do
  let redHerringFilePath = currentDirectory ++ redHerringTestExtension
  describe "Find strings when there are red herring files" $
    context "when we have files with other extensions" $
      it "Should return no views files" $
        findViewsFiles redHerringFilePath `shouldReturn` []

setupTestEnv :: FilePath -> IO ()
setupTestEnv currentDirectory = do
  mapM_ (\fp -> createDirectoryIfMissing True $ currentDirectory ++ fp) directoryExtensions
  mapM_ (createFileAndClose currentDirectory) fileExtensions

tearDownTestEnv :: FilePath -> IO ()
tearDownTestEnv currentDirectory = removeDirectoryRecursive (currentDirectory ++ testEnvExtension)

basicTestResults :: [FilePath]
basicTestResults = [basicTestExtension ++ "/myview.view"]

multiLevelResults :: [FilePath]
multiLevelResults = map (multiLevelExtension ++)
  ["/dir1/dir11/view2.view",
  "/dir1/view1.view",
  "/dir2/view3.view",
  "/dir3/view4.view",
  "/view0.view"]

multiFileResults :: [FilePath]
multiFileResults = map (multiFileExtension ++)
  ["/viewsFolder/view1.view",
  "/viewsFolder/view2.view",
  "/viewsFolder/view3.view"]

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
  multiFileExtension ++ "/viewsFolder",
  failTestExtension ++ "/dir1/dir11/dir111",
  failTestExtension ++ "/dir1/dir12",
  failTestExtension ++ "/dir2",
  failTestExtension ++ "/dir3",
  redHerringTestExtension,
  redHerringTestExtension ++ "/dir1/view"]

fileExtensions :: [FilePath]
fileExtensions = concat [basicTestResults,
  multiLevelResults,
  multiFileResults,
  redHerringFiles]

redHerringFiles :: [FilePath]
redHerringFiles = map (redHerringTestExtension ++)
  ["/mystrings.strings",
  "/mycolors.colors",
  "/myfonts.fonts",
  "/myalerts.alerts",
  "/myerrors.errors",
  "/view.xml",
  "/view.json",
  "/view"]

-- Setup Directory Structure
-- | testenv
-- -- | app1
-- -- -- myview.view
-- -- | app2
-- -- -- | view0.view
-- -- -- | dir1
-- -- -- -- | view1.view
-- -- -- -- | dir11
-- -- -- -- -- | view2.view
-- -- -- | dir2
-- -- -- -- | view3.view
-- -- -- | dir3
-- -- -- -- | view4.view
-- -- | app3
-- -- -- | viewsFolder
-- -- -- -- | view1.view
-- -- -- -- | view2.view
-- -- -- -- | view3.view
-- -- | app4
-- -- -- | dir1
-- -- -- -- | dir11
-- -- -- -- -- | dir111
-- -- -- -- | dir12
-- -- -- | dir2
-- -- -- | dir3
-- -- | app5
-- -- -- | mystrings.strings
-- -- -- | mycolors.colors
-- -- -- | myfonts.fonts
-- -- -- | myalerts.alerts
-- -- -- | myerrors.errors
-- -- -- | view.xml
-- -- -- | view.json
-- -- -- | view (no extension)
-- -- -- | dir1
-- -- -- -- | view (directory)
