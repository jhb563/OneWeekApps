module TestUtil (
  createFileAndClose,
  shouldReturnSorted,
  shouldReturnRights,
  shouldReturnWithoutErrors,
  filesShouldMatch,
  createResultsFiles,
  removeResultsFiles,
  removeFiles,
  removeDiffFiles
) where

import Data.List
import OWAObjcAbSyn
import OWAObjcPrint
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

-- Unwraps result of parsing and expects that we have a full list of objects
-- matching the given objects.
shouldReturnRights :: (Show b, Eq b) => IO (Either [a] [b]) -> [b] -> Expectation
shouldReturnRights returned expected = do
  result <- returned
  case result of
    Left _ -> fail "Parse Returned Errors"
    Right xs -> xs `shouldBe` expected

shouldReturnWithoutErrors :: Show b => IO (Either [a] [b]) -> Expectation
shouldReturnWithoutErrors wrappedVals = do
  result <- wrappedVals
  case result of
    Left _ -> fail "Parse Returned Errors"
    Right xs -> xs `shouldSatisfy` (not . null)

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

createResultsFiles :: FilePath -> [String] -> [ObjcFile] -> IO ()
createResultsFiles outputDirectory extensions structures = do
  let testFilePaths = map (outputDirectory ++) extensions
  mapM_ (uncurry printStructureToFile) (zip structures testFilePaths)

removeResultsFiles :: FilePath -> [String] -> IO ()
removeResultsFiles outputDirectory resultsFiles = removeFiles (map (outputDirectory ++) resultsFiles)

removeFiles :: [FilePath] -> IO ()
removeFiles = mapM_ removeFile

removeDiffFiles :: FilePath -> IO ()
removeDiffFiles directory = do
  allFiles <- listDirectory directory
  let diffFiles = map (\fp -> directory ++ '/':fp) (filter isDiffFile allFiles)
  removeFiles diffFiles

diffExtension :: String
diffExtension = ".diff"

isDiffFile :: FilePath -> Bool
isDiffFile filePath = case reverse filePath of
  'f':'f':'i':'d':'.':_ -> True
  _ -> False
