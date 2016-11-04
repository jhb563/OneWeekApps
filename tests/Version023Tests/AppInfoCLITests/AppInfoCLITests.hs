module AppInfoCLITests (
  runAppInfoCLITests
) where

import OWALib
import System.Directory (doesFileExist)
import System.IO
import Test.Hspec
import TestUtil

runAppInfoCLITests :: FilePath -> IO ()
runAppInfoCLITests currentDirectory = do
  let testDirectory = currentDirectory ++ testDirectoryExtension
  let appDirectory = currentDirectory ++ appDirectoryExtension
  hspec $ beforeAll_ (removeDiffFiles testDirectory >> removeDiffFiles appDirectory) $ do
    runAppInfoTest testDirectory "cancelAtAppName" "No input given"
    runAppInfoTest testDirectory "cancelAtAppPrefix" "Cancel at prefix entry"
    runAppInfoTest testDirectory "cancelAtName" "Cancel at name entry"
    runAppInfoTest testDirectory "cancelAtCompany" "Cancel at company entry"
    runAppInfoTest testDirectory "completeAppInfo" "Complete App Info"
    runAppInfoTest testDirectory "appInfoWithoutCompany" "App Info without company"
    runAppInfoTest testDirectory "blankAppName" "App Info with blank name"
    runAppInfoTest testDirectory "blankAppNameRecovery" "Recover from blank name"
    runAppInfoTest testDirectory "malformedPrefix1" "Too long prefix"
    runAppInfoTest testDirectory "malformedPrefixRecovery" "Recover from prefix error"

runAppInfoTest :: 
  FilePath -> -- Test Directory (will be appended to each filepath)
  String -> -- Prefix for test files
  String -> -- Description
  Spec
runAppInfoTest testDirectory testName description = do
  let (fullInput, fullTestOutput, fullResult, fullAppInfoTest, fullAppInfoResult) = fullTestFiles testDirectory testName
  beforeAll_ (openHandlesForFiles fullInput fullResult >>= runOWAWithHandles testDirectory)
    . afterAll_ (removeFiles [fullResult, fullAppInfoResult]) $
      describe description $ do
        it "Output should match" $ \_ -> do
          fullResult `filesShouldMatch` fullTestOutput
          maybeAppInfoTest fullAppInfoTest fullAppInfoResult
          
maybeAppInfoTest :: FilePath -> FilePath -> Expectation
maybeAppInfoTest testFile resultFile = do
  testExists <- doesFileExist testFile
  resultExists <- doesFileExist resultFile
  if not testExists && resultExists
    then expectationFailure "Should not have generated app.info!"
    else if not resultExists && testExists
      then expectationFailure "Should have generated app.info!"
      else if resultExists && testExists
        then resultFile `filesShouldMatch` testFile
        else return ()

fullTestFiles :: FilePath -> String -> (FilePath, FilePath, FilePath, FilePath, FilePath)
fullTestFiles testDirectory testName =
  ( testDirectory ++ "InputTestFiles/" ++ testName ++ ".in"
  , testDirectory ++ "OutputTestFiles/" ++ testName ++ ".out"
  , testDirectory ++ testName
  , testDirectory ++ "OutputTestFiles/" ++ testName ++ ".app.info" 
  , testDirectory ++ "app/app.info" )

openHandlesForFiles :: FilePath -> FilePath -> IO (Handle, Handle)
openHandlesForFiles inputFile outputFile = do
  iHandle <- openFile inputFile ReadMode
  oHandle <- openFile outputFile WriteMode
  return (iHandle, oHandle)

runOWAWithHandles :: FilePath -> (Handle, Handle) -> IO ()
runOWAWithHandles testDirectory (iHandle, oHandle) = do
  runOWA iHandle oHandle testDirectory ["new"]
  hClose iHandle
  hClose oHandle

testDirectoryExtension :: FilePath
testDirectoryExtension = "/tests/Version023Tests/AppInfoCLITests/"

appDirectoryExtension :: FilePath
appDirectoryExtension = testDirectoryExtension ++ "app/"

outputDirectoryExtension :: FilePath
outputDirectoryExtension = "/tests/Version023Tests/AppInfoCLITests/OutputTestFiles/"
