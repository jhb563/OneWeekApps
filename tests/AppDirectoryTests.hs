module AppDirectoryTests where

import OWAFileSearch
import System.Directory
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

setupTestEnv :: FilePath -> IO ()
setupTestEnv currentDirectory = do
    _ <- createDirectoryIfMissing True $ currentDirectory ++ redHerring2Extension 
    _ <- createDirectoryIfMissing True $ currentDirectory ++ deepAppExtension
    createDirectoryIfMissing True $ currentDirectory ++ app1Extension 

teardownTestEnv :: FilePath -> IO ()
teardownTestEnv currentDirectory = do
  removeDirectoryRecursive (currentDirectory ++ testEnvExtension)

runAppDirectoryTests :: FilePath -> IO ()
runAppDirectoryTests currentDirectory = do
    let testenvPath = currentDirectory ++ testEnvExtension
    let app1Path = currentDirectory ++ app1Extension
    let failStartPath = currentDirectory ++ failStartExtension
    let deepStartPath = currentDirectory ++ deepStartExtension
    let deepAppPath = currentDirectory ++ deepAppExtension
    _ <- setupTestEnv currentDirectory
    _ <- hspec $ do
        describe "Locate app Directory" $ do

            context "app directory is below current directory" $ do
              it "Directory should be found at /testenv/app" $ do
                findAppDirectory testenvPath `shouldBe` Just app1Path

            context "Current Directory is app" $ do
                it "Directory should be found as current" $ do
                  findAppDirectory app1Path `shouldBe` Just app1Path

            context "app directory is further below current directory" $ do
              it "Directory should be found at ./furtherBelowTest/app" $ do
                findAppDirectory deepStartPath `shouldBe` Just deepAppPath

        context "No app Directory" $ do
          it "Should return nothing" $ do
            findAppDirectory failStartPath `shouldBe` Nothing
    teardownTestEnv  currentDirectory
