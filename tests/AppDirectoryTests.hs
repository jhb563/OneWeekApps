module AppDirectoryTests where

import OWALib
import System.Directory
import Test.Hspec

-- Setup and Tear Down methods to reflect the directory structure under test

-- Setup will create a testenv folder with the following structure
-- | testenv
-- -- | app
-- -- | failTest
-- -- -- | redHerring
-- -- -- -- | redHerring2
-- -- | deeperTest
-- -- -- | app

-- Tear Down will destroy these directories

testEnvExtension :: FilePath
testEnvExtension = "/testenv"

app1Extension :: FilePath
app1Extension = "/testenv/app"

redHerring2Extension :: FilePath
redHerring2Extension = "/testenv/failTest/redHerring/redHerring2"

deepAppExtension :: FilePath
deepAppExtension = "/testenv/deeperTest/app"

setupTestEnv :: FilePath -> IO ()
setupTestEnv currentDirectory = do
    _ <- createDirectoryIfMissing True $ currentDirectory ++ redHerring2Extension 
    _ <- createDirectoryIfMissing True $ currentDirectory ++ deepAppExtension
    createDirectory $ currentDirectory ++ app1Extension 

teardownTestEnv :: FilePath -> IO ()
teardownTestEnv currentDirectory = removeDirectoryRecursive (currentDirectory ++ testEnvExtension)

runAppDirectoryTests :: FilePath -> IO ()
runAppDirectoryTests currentDirectory = do
    _ <- setupTestEnv currentDirectory
    _ <- hspec $ do
        describe "Locate app Directory" $ do
            context "Current Directory is app" $ do
                it "Directory should be found as current" $ do
                    pendingWith "Mind implement find!"

            context "app directory is below current directory" $ do
              it "Directory should be found at ./app" $ do
                  pendingWith "Must implement find!"

            context "app directory is further below current directory" $ do
              it "Directory should be found at ./furtherBelowTest/app" $ do
                  pendingWith "Must implement find!"

        context "No app Directory" $ do
          it "Exception should be thrown" $ do
              pendingWith "Must implement find!"
    teardownTestEnv
