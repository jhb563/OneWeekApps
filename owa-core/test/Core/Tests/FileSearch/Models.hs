-- This module will test the function:
-- findModelsFiles :: FilePath -> IO [FilePath]
-- from Core.FileSearch, which takes a directory and returns
-- a list of paths to .view files.

module Core.Tests.FileSearch.Models (
  runModelFileSearchTests
) where

import Data.List (sort)
import Test.Hspec

import Core.FileSearch (findModelsFiles)
import Core.Tests.Utils (shouldReturnSorted)

runModelFileSearchTests :: FilePath -> IO ()
runModelFileSearchTests currentDirectory = do
  let testDirectory = currentDirectory ++ testDirectoryBaseExtension
  hspec $ do
    testFilesExistFromSearch testDirectory "From the top directory" combo1
    testFilesExistFromSearch testDirectory "From 1 level down" combo2
    testFilesExistFromSearch testDirectory "From 2 levels down" combo3
    testFilesExistFromSearch testDirectory "From directory 2b" combo4
    testFilesExistFromSearch testDirectory "From 3 levels down" combo5
    testFilesExistFromSearch testDirectory "From directory 1b" combo6
    testFilesExistFromSearch testDirectory "From an empty directory" combo7

testFilesExistFromSearch :: FilePath -> String -> (FilePath, [FilePath]) -> Spec
testFilesExistFromSearch testDirectory contextDesc (searchExtension, expectedFiles) = do
  let searchDirectory = testDirectory ++ searchExtension
  let fullExpectedFiles = sort $ map (searchDirectory ++) expectedFiles
  describe "Search for models files" $
    context contextDesc $
      it "Should find the same files" $
        findModelsFiles searchDirectory `shouldReturnSorted` fullExpectedFiles 

combo1 :: (FilePath, [FilePath])
combo1 = (testDirectoryExtension1, testModels1)

combo2 :: (FilePath, [FilePath])
combo2 = (testDirectoryExtension2, testModels2)

combo3 :: (FilePath, [FilePath])
combo3 = (testDirectoryExtension3, testModels3)

combo4 :: (FilePath, [FilePath])
combo4 = (testDirectoryExtension4, testModels4)

combo5 :: (FilePath, [FilePath])
combo5 = (testDirectoryExtension5, testModels5)

combo6 :: (FilePath, [FilePath])
combo6 = (testDirectoryExtension6, testModels6)

combo7 :: (FilePath, [FilePath])
combo7 = (testDirectoryExtension7, testModels7)

testDirectoryBaseExtension :: FilePath
testDirectoryBaseExtension = "/test/Core/Tests/FileSearch"

testDirectoryExtension1 :: FilePath
testDirectoryExtension1 = "/modelTestDirs"

testDirectoryExtension2 :: FilePath
testDirectoryExtension2 = "/modelTestDirs/level1"

testDirectoryExtension3 :: FilePath
testDirectoryExtension3 = "/modelTestDirs/level1/level2"

testDirectoryExtension4 :: FilePath
testDirectoryExtension4 = "/modelTestDirs/level1/level2b"

testDirectoryExtension5 :: FilePath
testDirectoryExtension5 = "/modelTestDirs/level1/level2b/level3"

testDirectoryExtension6 :: FilePath
testDirectoryExtension6 = "/modelTestDirs/level1b"

testDirectoryExtension7 :: FilePath
testDirectoryExtension7 = "/modelTestDirs/empty"

testModels1 :: [FilePath]
testModels1 = 
  [ "/model1.model"
  , "/level1/model2.model"
  , "/level1/model3.model"
  , "/level1/level2/model4.model"
  , "/level1/level2b/model5.model"
  , "/level1/level2b/level3/model6.model" 
  , "/level1b/model7.model" 
  ]

testModels2 :: [FilePath]
testModels2 = 
  [ "/model2.model"
  , "/model3.model"
  , "/level2/model4.model"
  , "/level2b/model5.model"
  , "/level2b/level3/model6.model" 
  ]

testModels3 :: [FilePath]
testModels3 = ["/model4.model"]

testModels4 :: [FilePath]
testModels4 = ["/model5.model", "/level3/model6.model"]

testModels5 :: [FilePath]
testModels5 = ["/model6.model"]

testModels6 :: [FilePath]
testModels6 = ["/model7.model"]

testModels7 :: [FilePath]
testModels7 = []

-- Setup Directory Structure
-- | modelTestDirs
-- -- | model1.model
-- -- | level1
-- -- -- | model2.model
-- -- -- | model3.model
-- -- -- | level2
-- -- -- -- | model4.model
-- -- -- | level2b
-- -- -- -- | model5.model
-- -- -- -- | level3
-- -- -- -- -- | model6.model
-- -- | level1b
-- -- -- | model7.model
-- -- | empty
