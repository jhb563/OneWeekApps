-- OWAObjcPrint will expose the method
-- printStructureToFile :: ObjcFile -> FilePath -> IO ()
-- which takes an objective C file structure and a filepath
-- and prints the file structure to the given file

module FontPrintTests (
  runFontPrintTests
) where

import OWAFontObjc
import OWAObjcAbSyn
import OWAObjcPrint
import TestFonts
import TestUtil
import Test.Hspec
runFontPrintTests :: FilePath -> IO ()
runFontPrintTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/tests/FontTests/FontOutputFiles/"
  hspec $
    beforeAll_ (removeDiffFiles testDirectory) $
    beforeAll_ (createResultsFiles testDirectory resultsFiles testFileStructures)
    . afterAll_ (removeResultsFiles testDirectory resultsFiles) $ do
      emptyCategoryTests testDirectory 
      fullCategoryTests testDirectory

emptyCategoryTests :: FilePath -> Spec
emptyCategoryTests testDirectory = describe "Print File Structure for Empty Category" $ do
  it "The printed header file should match" $
    (testDirectory ++ emptyHeaderResultFile) `filesShouldMatch` 
      (testDirectory ++ emptyHeaderTestFile)
  
  it "The printed implementation file should match" $
    (testDirectory ++ emptyImplementationResultFile) `filesShouldMatch`
      (testDirectory ++ emptyImplementationTestFile) 

fullCategoryTests :: FilePath -> Spec
fullCategoryTests testDirectory = describe "Print File Structure for Normal Font Category" $ do
  it "The printed header file should match" $
    (testDirectory ++ headerResultFile) `filesShouldMatch`
      (testDirectory ++ headerTestFile) 

  it "The printed implementation file should match" $
    (testDirectory ++ implementationResultFile) `filesShouldMatch`
      (testDirectory ++ implementationTestFile)

resultsFiles :: [String]
resultsFiles = [emptyHeaderResultFile,
  emptyImplementationResultFile,
  headerResultFile,
  implementationResultFile]

testFileStructures :: [ObjcFile]
testFileStructures = [objcHeaderFromFonts "EmptyCategory" [],
  objcImplementationFromFonts "EmptyCategory" [], 
  objcHeaderFromFonts "MyAppFonts" allTestFonts, 
  objcImplementationFromFonts "MyAppFonts" allTestFonts]

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

