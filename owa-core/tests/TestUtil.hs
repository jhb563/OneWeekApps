module TestUtil (
  shouldReturnSorted,
  shouldReturnRights,
  shouldReturnLefts,
  shouldReturnWithoutErrors,
  shouldMatchError,
  filesShouldMatch,
  createFileAndClose,
  createResultsFiles,
  createSwiftResultsFiles,
  removeResultsFiles,
  removeFiles,
  removeDiffFiles
) where

import Control.Monad (when)
import Data.List
import System.Directory
import System.IO
import System.Process
import Test.Hspec
import Text.Parsec.Error
import Text.Parsec.Pos

import Model.OWAParseError
import Objc.AbSyn
import Objc.Print
import OWASwiftAbSyn
import OWASwiftPrint

--------------------------------------------------------------------------------
------------------------------EXPECTATION COMBINATORS---------------------------
--------------------------------------------------------------------------------

-- Same as shouldReturn, but takes a list and sorts the entries first 
-- for testing consistency
shouldReturnSorted :: (Show a, Ord a) => IO [a] -> [a] -> Expectation
shouldReturnSorted returned expected = do
  actual <- returned
  sort actual `shouldBe` expected

-- Unwraps result of parsing and expects that we have a full list of objects
-- matching the given objects.
shouldReturnRights :: (Show a, Show b, Eq b) => IO (Either [a] b) -> b -> Expectation
shouldReturnRights returned expected = do
  result <- returned
  case result of
    Left errors -> fail ("Parse Returned Errors: " ++ show errors)
    Right xs -> xs `shouldBe` expected

-- Unwraps result of parsing and expects that we have a full list of failures 
-- matching the given error objects.
shouldReturnLefts :: (Show a, Eq a) => IO (Either a b) -> a -> Expectation
shouldReturnLefts returned expected = do
  result <- returned
  case result of
    Right _ -> fail "Parse Returned Completed objects"
    Left xs -> xs `shouldBe` expected

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

shouldMatchError :: IO (Either [OWAParseError] b) -> SourcePos -> Expectation
shouldMatchError returned srcPos = do
  result <- returned
  case result of
    Right _ -> fail "Parse Returned Completed Objects"
    Left [ParsecError parseError] ->
      errorPos parseError `shouldBe` srcPos
    _ -> fail "Incorrect number or format of errors"

--------------------------------------------------------------------------------
------------------------------FILE MANIPULATION---------------------------------
--------------------------------------------------------------------------------

-- Takes a base filePath and a list of relative paths to files. 
-- Creates the empty files.
createFileAndClose :: FilePath -> FilePath -> IO ()
createFileAndClose base extension = do
  handle <- openFile (base ++ extension) WriteMode
  hClose handle

createResultsFiles :: FilePath -> [String] -> [ObjcFile] -> IO ()
createResultsFiles outputDirectory extensions structures = do
  let testFilePaths = map (outputDirectory ++) extensions
  mapM_ (uncurry printStructureToFile) (zip structures testFilePaths)

createSwiftResultsFiles :: FilePath -> [String] -> [SwiftFile] -> IO ()
createSwiftResultsFiles outputDirectory extensions structures = do
  let testFilePaths = map (outputDirectory ++) extensions
  mapM_ (uncurry printSwiftStructureToFile) (zip structures testFilePaths)

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
