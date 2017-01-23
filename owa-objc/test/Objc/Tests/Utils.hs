module Objc.Tests.Utils
  ( filesShouldMatch 
  , createResultsFiles
  , removeDiffFiles
  , removeResultsFiles )
  where

import Control.Monad (when)
import System.Directory
import System.IO
import System.Process
import Test.Hspec

import Objc.AbSyn
import Objc.Print

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

--------------------------------------------------------------------------------
------------------------------FILE MANIPULATION---------------------------------
--------------------------------------------------------------------------------

createResultsFiles :: FilePath -> [String] -> [ObjcFile] -> IO ()
createResultsFiles outputDirectory extensions structures = do
  let testFilePaths = map (outputDirectory ++) extensions
  mapM_ (uncurry printStructureToFile) (zip structures testFilePaths)

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
