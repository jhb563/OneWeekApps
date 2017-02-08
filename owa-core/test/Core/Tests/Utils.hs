module Core.Tests.Utils 
  ( createFileAndClose 
  , shouldReturnSorted 
  , filesShouldMatch 
  , removeDiffFiles
  , removeFiles 
  , removeResultsFiles )
  where

import Control.Monad (when)
import Data.List (sort)
import System.Directory
import System.IO
import System.Process
import Test.Hspec

-- Takes a base filePath and a list of relative paths to files. 
-- Creates the empty files.
createFileAndClose :: FilePath -> FilePath -> IO ()
createFileAndClose base extension = do
  handle <- openFile (base ++ extension) WriteMode
  hClose handle

-- Same as shouldReturn, but takes a list and sorts the entries first 
-- for testing consistency
shouldReturnSorted :: (Show a, Ord a) => IO [a] -> [a] -> Expectation
shouldReturnSorted returned expected = do
  actual <- returned
  sort actual `shouldBe` expected

filesShouldMatch :: FilePath -> FilePath -> Expectation
filesShouldMatch actualFile expectedFile = do
  actualString <- readFile actualFile
  expectedString <- readFile expectedFile
  if actualString == expectedString
    then actualString `shouldBe` expectedString
    else do
      (_,stdOutHandler,_,_) <- runInteractiveProcess "diff" [actualFile, expectedFile] Nothing Nothing
      diffContents <- hGetContents stdOutHandler
      writeFile (actualFile ++ diffExtension) diffContents
      actualString `shouldBe` expectedString

removeResultsFiles :: FilePath -> [String] -> IO ()
removeResultsFiles outputDirectory resultsFiles = removeFiles (map (outputDirectory ++) resultsFiles)

removeFiles :: [FilePath] -> IO ()
removeFiles = mapM_ (\file -> do
  exists <- doesFileExist file
  when exists $ removeFile file)

removeDiffFiles :: FilePath -> IO ()
removeDiffFiles directory = do
  dirExists <- doesDirectoryExist directory
  when dirExists $ do
    allFiles <- listDirectory directory
    let diffFiles = map (\fp -> directory ++ '/':fp) (filter isDiffFile allFiles)
    removeFiles diffFiles

diffExtension :: String
diffExtension = ".diff"

isDiffFile :: FilePath -> Bool
isDiffFile filePath = case reverse filePath of
  'f':'f':'i':'d':'.':_ -> True
  _ -> False
