-- This module will test the function:
-- findAppInfoFile :: FilePath -> IO (Maybe FilePath)
-- from OWAFileSearch, which takes a directory and returns the path to
-- the app.info file, as well as the function:
-- parseAppInfoFromFile :: FilePath -> IO Either [OWAParseError] OWAAppInfo
-- in the module OWAAppInfoParser which will take the possible 
-- filepath of the app info and returns a structure with the app info, 
-- with default values filled in if there is no filepath.

module DefaultsFileTests (
  runDefaultsFileTests
) where

import Test.Hspec
import System.Directory

import OWAAppInfoParser
import OWAFileSearch
import TestAppInfo
import TestDefaultErrors
import TestUtil

runDefaultsFileTests :: FilePath -> IO ()
runDefaultsFileTests currentDirectory = hspec $
  beforeAll_ (setupTestEnv currentDirectory)
  . afterAll_ (teardownTestEnv currentDirectory) $ do
    let parseDirectory = currentDirectory ++ "/tests/Version015Tests/DefaultsFileTests/ParseFiles"
    appInfoSearchTests currentDirectory 
    defaultPassTests parseDirectory
    defaultFailTests parseDirectory

appInfoSearchTests :: FilePath -> Spec
appInfoSearchTests currentDirectory = do
  let startPath1 = currentDirectory ++ appInfo1FolderExtension
  let startPath2 = currentDirectory ++ appInfo2FolderExtension
  let startPath3 = currentDirectory ++ appInfo3FolderExtension
  let startPath4 = currentDirectory ++ appInfo4FolderExtension
  describe "Find App Info File" $ do
    context "when in the immediate directory being searched" $
      it "Should return the app info file" $
        findAppInfoFile startPath1 `shouldReturn` (Just $ currentDirectory ++ appInfo1Extension)
    
    context "when one folder below the directory" $
      it "Should return the app info file" $
        findAppInfoFile startPath2 `shouldReturn` (Just $ currentDirectory ++ appInfo2Extension)

    context "when multiple folders below with a red herring directory" $
      it "Should return the app info file" $
        findAppInfoFile startPath3 `shouldReturn` (Just $ currentDirectory ++ appInfo3Extension)

    context "when there is no file" $
      it "Should return Nothing" $
        findAppInfoFile startPath4 `shouldReturn` Nothing

defaultPassTests :: FilePath -> Spec
defaultPassTests testDirectory = do
  let testFile1 = testDirectory ++ defaultPassExtension1
  let testFile2 = testDirectory ++ defaultPassExtension2
  let testFile3 = testDirectory ++ defaultPassExtension3
  let testFile4 = testDirectory ++ defaultPassExtension4
  let testFile5 = testDirectory ++ defaultPassExtension5
  describe "Parse app info from correctly formatted files" $ do
    context "with a Author, Created date, and Company Name" $
      it "Should return the app info contained in the file" $
        parseAppInfoFromFile testFile1 `shouldReturnRights` appInfo1

    context "with a Author, Created date, and Company Name in a different order" $
      it "Should return the app info contained in the file" $
        parseAppInfoFromFile testFile2 `shouldReturnRights` appInfo2

    context "with no company name" $
      it "Should return the app info contained in the file but with no company" $
        parseAppInfoFromFile testFile3 `shouldReturnRights` appInfo3

    context "with no author name" $
      it "Should return the app info contained in the file but with a blank author name" $
        parseAppInfoFromFile testFile4 `shouldReturnRights` appInfo4

    context "with comments both before and inline" $
      it "Should return the app info contained in the file ignoring the comments" $
        parseAppInfoFromFile testFile5 `shouldReturnRights` appInfo5

defaultFailTests :: FilePath -> Spec
defaultFailTests testDirectory = do
  let testFile1 = testDirectory ++ defaultFailExtension1
  let testFile2 = testDirectory ++ defaultFailExtension2
  let testFile3 = testDirectory ++ defaultFailExtension3
  let testFile4 = testDirectory ++ defaultFailExtension4
  let testFile5 = testDirectory ++ defaultFailExtension5
  let testFile6 = testDirectory ++ defaultFailExtension6
  let testFile7 = testDirectory ++ defaultFailExtension7
  let testFile8 = testDirectory ++ defaultFailExtension8
  let testFile9 = testDirectory ++ defaultFailExtension9
  describe "Parse app info from incorrectly formatted files" $ do
    context "when a keyword is not capitalized" $
      it "Should return a parse error highlighting the keyword" $
        parseAppInfoFromFile testFile1 `shouldMatchError` appError1

    context "when a fictional keyword is used" $
      it "Should return a parse error highlighting the keyword" $
        parseAppInfoFromFile testFile2 `shouldMatchError` appError2

    context "when a colon is not used after a keyword" $
      it "Should return a parse error highlighting the keyword" $
        parseAppInfoFromFile testFile3 `shouldMatchError` appError3

    context "when the date is poorly formatted" $
      it "Should return a parse error highlighting the date" $
        parseAppInfoFromFile testFile4 `shouldMatchError` appError4

    context "when the app name is omitted" $
      it "Should return an object error highlighting the need for an app name" $
        parseAppInfoFromFile testFile5 `shouldReturnLefts` [appError5]

    context "when the prefix is omitted" $
      it "Should return an object error highlighting the need for a prefix" $
        parseAppInfoFromFile testFile6 `shouldReturnLefts` [appError6]

    context "when the prefix is too short" $
      it "Should return an object error highlighting the incorrect prefix" $
        parseAppInfoFromFile testFile7 `shouldMatchError` appError7

    context "when the prefix is too long" $
      it "Should return an object error highlighting the incorrect prefix" $
        parseAppInfoFromFile testFile8 `shouldMatchError` appError8

    context "when the prefix is lowercase" $
      it "Should return an object error highlighting the incorrect prefix" $
        parseAppInfoFromFile testFile9 `shouldMatchError` appError9

setupTestEnv :: FilePath -> IO ()
setupTestEnv currentDirectory = do
  mapM_ (\fp -> createDirectoryIfMissing True $ currentDirectory ++ fp) directorySearchFolderExtensions
  mapM_ (createFileAndClose currentDirectory) directorySearchFileExtensions

teardownTestEnv :: FilePath -> IO ()
teardownTestEnv currentDirectory = removeDirectoryRecursive (currentDirectory ++ testEnvFolderExtension)

defaultPassExtension1 :: String
defaultPassExtension1 = "/defaultPassTest1.info"

defaultPassExtension2 :: String
defaultPassExtension2 = "/defaultPassTest2.info"

defaultPassExtension3 :: String
defaultPassExtension3 = "/defaultPassTest3.info"

defaultPassExtension4 :: String
defaultPassExtension4 = "/defaultPassTest4.info"

defaultPassExtension5 :: String
defaultPassExtension5 = "/defaultPassTest5.info"

defaultFailExtension1 :: String
defaultFailExtension1 = "/defaultFailTest1.info"

defaultFailExtension2 :: String
defaultFailExtension2 = "/defaultFailTest2.info"

defaultFailExtension3 :: String
defaultFailExtension3 = "/defaultFailTest3.info"

defaultFailExtension4 :: String
defaultFailExtension4 = "/defaultFailTest4.info"

defaultFailExtension5 :: String
defaultFailExtension5 = "/defaultFailTest5.info"

defaultFailExtension6 :: String
defaultFailExtension6 = "/defaultFailTest6.info"

defaultFailExtension7 :: String
defaultFailExtension7 = "/defaultFailTest7.info"

defaultFailExtension8 :: String
defaultFailExtension8 = "/defaultFailTest8.info"

defaultFailExtension9 :: String
defaultFailExtension9 = "/defaultFailTest9.info"

testEnvFolderExtension :: FilePath
testEnvFolderExtension = "/testenv"

appInfo1FolderExtension :: FilePath
appInfo1FolderExtension = "/testenv/app1"

appInfo2FolderExtension :: FilePath
appInfo2FolderExtension = "/testenv/app2"

appInfo2FolderEndExtension :: FilePath
appInfo2FolderEndExtension = "/testenv/app2/src"

appInfo3FolderExtension :: FilePath
appInfo3FolderExtension = "/testenv/app3"

appInfo3FolderEndExtension :: FilePath
appInfo3FolderEndExtension = "/testenv/app3/Here/OneMore"

appInfo4FolderExtension :: FilePath
appInfo4FolderExtension = "/testenv/app4"

appInfo1Extension :: FilePath
appInfo1Extension = "/testenv/app1/app.info"

appInfo2Extension :: FilePath
appInfo2Extension = "/testenv/app2/src/app.info"

appInfo3Extension :: FilePath
appInfo3Extension = "/testenv/app3/Here/OneMore/app.info"

appInfo3RandomExtension :: FilePath
appInfo3RandomExtension  = "/testenv/app3/notHere"

appInfoFailExtension1 :: FilePath
appInfoFailExtension1 = "/testenv/app4/nope/level2"

appInfoFailExtension2 :: FilePath
appInfoFailExtension2 = "/testenv/app4/orThis/level3/level4"

directorySearchFolderExtensions :: [FilePath]
directorySearchFolderExtensions = [appInfo1FolderExtension,
  appInfo2FolderEndExtension,
  appInfo3FolderEndExtension,
  appInfo3RandomExtension,
  appInfoFailExtension1,
  appInfoFailExtension2]

directorySearchFileExtensions :: [FilePath]
directorySearchFileExtensions = [appInfo1Extension,
  appInfo2Extension,
  appInfo3Extension]

-- Setup Directory Structure
-- | testenv
-- -- | app1
-- -- -- | app.info
-- -- | app2
-- -- -- | src
-- -- -- -- | app.info
-- -- | app3
-- -- -- | notHere
-- -- -- | Here
-- -- -- -- | OneMore
-- -- -- -- -- app.info
-- -- | app4
-- -- -- | nope
-- -- -- -- | level2
-- -- -- | orThis
-- -- -- -- | level3
-- -- -- -- -- | level4
