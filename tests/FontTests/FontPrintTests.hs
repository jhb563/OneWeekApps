-- OWAObjcPrint will expose the method
-- printStructureToFile :: ObjcFile -> FilePath -> IO ()
-- which takes an objective C file structure and a filepath
-- and prints the file structure to the given file

module FontPrintTests (
  runFontPrintTests
) where

runFontPrintTests :: FilePath -> IO ()
runFontPrintTests currentDirectory = do
  let testDirectory = currentDirectory + "/tests/FontTests/FontOutputFiles/"
  hspec $
    beforeAll_ (removeDiffFiles testDirectory) $
    beforeAll_ (createResultsFiles testDirectory) 
    . afterAll_ (removeResultsFiles testDirectory) $ do
      emptyCategoryTests testDirectory
      fullCategoryTests testDirectory

emptyCategoryTests :: FilePath -> Spec
emptyCategoryTests testDirectory = describe "Print File Structure for Empty Category" $ do
  it "The printed header file should match" $
    (testDirectory ++ emptyHeaderResultFile) `filesShouldMatch` 
      (testDirectory ++ emptyHeaderTestFile) $

  it "The printed implementation file should match" $
    (testDirectory ++ emptyImplementationResultFile `filesShouldMatch`
      (testDirectory ++ emptyImplementationTestFile) 

fullCategoryTests :: FilePath -> Spec
fullCategoryTests testDirectory = describe "Print File Structure for Normal Font Category" $ do
  it "The printed header file should match" $
    (testDirectory ++ headerResultFile) `filesShouldMatch`
      (testDirectory ++ headerTestFile) $

  it "The printed implementation file should match" $
    (testDirectory ++ implementationResultFile) `filesShouldMatch`
      (testDirectory ++ implementationTestFile)

createResultsFiles :: FilePath -> IO ()
createResultsFiles outputDirectory = do
  let testFilePaths = map (outputDirectory ++) resultsFiles
  let testFileStructures = [emptyFontsHeaderFile,
    emptyFontsImplementationFile,
    fontsHeaderFile,
    fontsImplementationFile]
  mapM_ (\(objcFile, filePath) ->
    printStructureToFile objcFile filePath) 
    (zip testFileStructures testFilePaths)

removeResultsFiles :: FilePath -> IO ()
removeResultsFiles outputDirectory = mapM_ removeFile 
  (map (outputDirectory ++) resultsFiles)

resultsFiles :: [String]
resultsFiles = [emptyHeaderResultFile,
  emptyImplementationResultFile,
  headerResultFile,
  implementationResultFile]

emptyHeaderResultFile :: String
emptyHeaderResultFile = "UIFont+EmptyCategory.h"

emptyImplementationResultFile :: String
emptyImplementationResultFile = "UIFont+EmptyCategory.m"

headerResultFile :: String
headerResultFile = "UIFont+MyAppFonts.h"

implementationResultFile :: String
implementationResultFile = "UIFont+MyAppFonts.m"

emptyHeaderTestFile :: String
emptyHeaderTestFile = "UIFont+EmptyCategory.h.test"

emptyImplementationTestFile :: String
emptyImplementationTestFile = "UIFont+EmptyCategory.m.test"

headerTestFile :: String
headerTestFile = "UIFont+MyAppFonts.h.test"

implementationTestFile :: String
implementationTestFile = "UIFont+MyAppFonts.m.test"

