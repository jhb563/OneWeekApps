-- OWAFileSearch will expose the method:
-- findAppDirectory :: FilePath -> IO (Maybe FilePath)
-- We pass in a starting directory, and it will do a BFS
-- for the first directory called 'app'

module AppDirectoryTests (
  runAppDirectoryTests
) where

import System.Directory
import Test.Hspec

import OWAFileSearch
import TestUtil

runAppDirectoryTests :: FilePath -> IO ()
runAppDirectoryTests currentDirectory = hspec $ 
  beforeAll_ (setupTestEnv currentDirectory)
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

    context "when app directory is below current directory" $
      it "Directory should be found at /testenv/app" $
        findAppDirectory testenvPath `shouldReturn` Just app1Path

    context "when current Directory is app" $
        it "Directory should be found as current" $
          findAppDirectory app1Path `shouldReturn` Just app1Path

    context "when app directory is further below current directory" $
      it "Directory should be found at testenv/deeperTest/deeperTest2/app" $
        findAppDirectory deepStartPath `shouldReturn` Just deepAppPath

findAppDirectoryFailSpec :: FilePath -> Spec
findAppDirectoryFailSpec currentDirectory = do
  let failStartPath = currentDirectory ++ failStartExtension
  let appFailPath = currentDirectory ++ falseAppNamesStart
  describe "Fail to find app directory" $ do

    context "when there is no app Directory" $
      it "Should return nothing" $
        findAppDirectory failStartPath `shouldReturn` Nothing

    context "when there are directories beginning, ending, and containing app, and files even matching app" $
      it "Should return nothing" $
          findAppDirectory appFailPath `shouldReturn` Nothing

    context "when there is a capitalized app directory App" $
      it "Should return nothing" $
        findAppDirectory capitalAppStart `shouldReturn` Nothing

setupTestEnv :: FilePath -> IO ()
setupTestEnv currentDirectory = do
  _ <- createDirectoryIfMissing True $ currentDirectory ++ redHerring2Extension 
  _ <- createDirectoryIfMissing True $ currentDirectory ++ deepAppExtension
  _ <- createDirectoryIfMissing True $ currentDirectory ++ app1Extension 
  _ <- createDirectoryIfMissing True $ currentDirectory ++ capitalAppExtension
  let falseAppNamesFullPath = currentDirectory ++ falseAppNamesExtension
  _ <- createDirectoryIfMissing True falseAppNamesFullPath
  mapM_ (createFileAndClose falseAppNamesFullPath) falseAppFileExtensions

teardownTestEnv :: FilePath -> IO ()
teardownTestEnv currentDirectory = removeDirectoryRecursive (currentDirectory ++ testEnvExtension)

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

capitalAppStart :: FilePath
capitalAppStart = "/testenv/failureCases/AppTest"

capitalAppExtension :: FilePath
capitalAppExtension = "/testenv/failureCases/AppTest/App"

falseAppNamesStart :: FilePath
falseAppNamesStart = "/testenv/failureCases"

falseAppNamesExtension :: FilePath
falseAppNamesExtension = "/testenv/failureCases/appTastic/buildAnApp/myappium"

falseAppFileExtensions :: [FilePath]
falseAppFileExtensions = ["/app", "/myappium", "appTopic", "thingWithapp"]

-- Setup Directory Structure
-- | testenv
-- -- | app
-- -- | failTest
-- -- -- | redHerring
-- -- -- -- | redHerring2
-- -- | deeperTest
-- -- -- | deeperTest2
-- -- --  -- | app
-- -- | failureCases
-- -- -- | AppTest
-- -- -- -- App
-- -- -- | appTastic
-- -- -- -- | buildAnApp
-- -- -- -- -- | myappium
-- -- -- -- -- -- | app (file)
-- -- -- -- -- -- | myappium (file)
-- -- -- -- -- -- | appTopic (file)
-- -- -- -- -- -- | thingwithapp (file)
