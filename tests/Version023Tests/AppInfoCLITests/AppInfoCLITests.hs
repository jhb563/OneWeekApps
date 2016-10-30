module AppInfoCLITests (
  runAppInfoCLITests
) where

import OWALib
import System.IO
import Test.Hspec
import TestUtil

runAppInfoCLITests :: FilePath -> IO ()
runAppInfoCLITests currentDirectory = do
  let testDirectory = currentDirectory ++ testDirectoryExtension
  hspec $ beforeAll_ (removeDiffFiles testDirectory) $ do
    runAppInfoTest testDirectory test1 "First Test"

-- Each Test Consists Of:
-- An input file which will be the handle fed into the CLI
-- An output file for what we expect to be printed
-- A possible output file for an app.info file we expect to be created
type AppInfoTest = (FilePath, FilePath, Maybe FilePath)

test1 :: AppInfoTest
test1 = ("InputTestFiles/test1.in", "OutputTestFiles/test1.out", Nothing)

runAppInfoTest :: 
  FilePath -> -- Test Directory (will be appended to each filepath)
  AppInfoTest -> -- Input and results files
  String -> -- Description
  Spec
runAppInfoTest testDirectory testFiles description = do
  let (fullInput, fullOutput, maybeFullAppInfo) = fullTestFiles testDirectory testFiles
  let fullResult = testDirectory ++ outputResultFile
  beforeAll_ (openHandlesForFiles fullInput fullResult >>= runOWAWithHandles testDirectory)
    . afterAll_ (removeFiles [fullResult]) $
      describe description $ do
        it "Output should match" $ \_ ->
          fullResult `filesShouldMatch` fullOutput

fullTestFiles :: FilePath -> AppInfoTest -> AppInfoTest
fullTestFiles testDirectory (inputFile, outputFile, maybeAppInfoFile) =
  ( testDirectory ++ inputFile
  , testDirectory ++ outputFile
  , (testDirectory ++) <$> maybeAppInfoFile )

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

outputResultFile :: FilePath
outputResultFile = "output.result"
