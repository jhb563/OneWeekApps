module AppDirectoryTests where

import OWAFileSearch
import System.Directory
import System.IO
import Test.Hspec

-- Setup will create a testenv folder with the following structure
-- | testenv
-- -- | app
-- -- | failTest
-- -- -- | redHerring
-- -- -- -- | redHerring2
-- -- | deeperTest
-- -- -- | deeperTest2
-- -- --  -- | app
-- -- | failureCases
-- -- -- | appTastic
-- -- -- -- | buildAnApp
-- -- -- -- -- | myappium
-- -- -- -- -- -- | app (file)
-- -- -- -- -- -- | myappium (file)
-- -- -- -- -- -- | appTopic (file)
-- -- -- -- -- -- | thingwithapp (file)

-- Tear Down will destroy these directories

testEnvExtension :: FilePath
testEnvExtension = "/testenv"

app1Extension :: FilePath
app1Extension = "/testenv/app"

failStartExtension :: FilePath
failStartExtension = "/testenv/failTest"

redHerring2Extension :: FilePath
redHerring2Extension = "/testenv/failTest/redHerring/redHerring2"

deepStartExtension :: FilePath
deepStartExtension = "/testenv/deeperTest"

deepAppExtension :: FilePath
deepAppExtension = "/testenv/deeperTest/deeperTest2/app"

falseAppNamesStart :: FilePath
falseAppNamesStart = "/testenv/failureCases"

falseAppNamesExtension :: FilePath
falseAppNamesExtension = "/testenv/failureCases/appTastic/buildAnApp/myappium"

falseAppFileExtensions :: [FilePath]
falseAppFileExtensions = ["/app", "/myappium", "appTopic", "thingWithapp"]

setupTestEnv :: FilePath -> IO ()
setupTestEnv currentDirectory = do
    _ <- createDirectoryIfMissing True $ currentDirectory ++ redHerring2Extension 
    _ <- createDirectoryIfMissing True $ currentDirectory ++ deepAppExtension
    _ <- createDirectoryIfMissing True $ currentDirectory ++ app1Extension 
    let falseAppNamesFullPath = currentDirectory ++ falseAppNamesExtension
    _ <- createDirectoryIfMissing True $ falseAppNamesFullPath
    sequence_ $ map (createFileAndClose falseAppNamesFullPath) falseAppFileExtensions

createFileAndClose :: FilePath -> FilePath -> IO ()
createFileAndClose base extension = do
                              handle <- openFile (base ++ extension) WriteMode
                              hClose handle

teardownTestEnv :: FilePath -> IO ()
teardownTestEnv currentDirectory = do
  removeDirectoryRecursive (currentDirectory ++ testEnvExtension)

runAppDirectoryTests :: FilePath -> IO ()
runAppDirectoryTests currentDirectory = do
  hspec $ beforeAll_ (setupTestEnv currentDirectory)
     . afterAll_ (teardownTestEnv currentDirectory) $ do
    _ <- findAppDirectorySuccessSpec currentDirectory
    findAppDirectoryFailSpec currentDirectory

findAppDirectorySuccessSpec :: FilePath -> Spec
findAppDirectorySuccessSpec currentDirectory = do
  let testenvPath = currentDirectory ++ testEnvExtension
  let app1Path = currentDirectory ++ app1Extension
  let deepStartPath = currentDirectory ++ deepStartExtension
  let deepAppPath = currentDirectory ++ deepAppExtension
  describe "Locate app Directory" $ do

        context "when app directory is below current directory" $ do
          it "Directory should be found at /testenv/app" $ do
            findAppDirectory testenvPath `shouldReturn` Just app1Path

        context "when current Directory is app" $ do
            it "Directory should be found as current" $ do
              findAppDirectory app1Path `shouldReturn` Just app1Path

        context "when app directory is further below current directory" $ do
          it "Directory should be found at testenv/deeperTest/deeperTest2/app" $ do
            findAppDirectory deepStartPath `shouldReturn` Just deepAppPath

findAppDirectoryFailSpec :: FilePath -> Spec
findAppDirectoryFailSpec currentDirectory = do
  let failStartPath = currentDirectory ++ failStartExtension
  let appFailPath = currentDirectory ++ falseAppNamesStart
  describe "Fail to find app directory" $ do

    context "when there is no app Directory" $ do
      it "Should return nothing" $ do
        findAppDirectory failStartPath `shouldReturn` Nothing

      context "when there are directories beginning, ending, and containing app, and files even matching app" $ do
        it "Should return nothing" $ do
            findAppDirectory appFailPath `shouldReturn` Nothing

