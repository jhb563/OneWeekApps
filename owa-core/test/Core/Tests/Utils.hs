module Core.Tests.Utils 
  ( createFileAndClose 
  , shouldReturnSorted )
  where

import Data.List (sort)
import System.Directory
import System.IO
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
