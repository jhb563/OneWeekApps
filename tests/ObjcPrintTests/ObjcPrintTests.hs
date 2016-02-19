-- OWAObjcPrint will expose the method
-- printStructureToFile :: ObjcFile -> FilePath -> IO ()
-- which takes an objective C file structure and a filepath
-- and prints the file structure to the given file

module ObjcPrintTests (
  runObjcPrintTests
) where

import OWAObjcAbSyn
import OWAObjcPrint
import Test.Hspec

runObjcPrintTests :: FilePath -> IO ()
runObjcPrintTests currentDirectory = hspec $ do
  let testDirectory = currentDirectory ++ "/tests/ObjcPrintTests/"
  blockCommentTests testDirectory
  importTests testDirectory
  interfaceTests testDirectory
  implementationTests testDirectory
  integrationTests testDirectory

blockCommentTests :: FilePath -> Spec
blockCommentTests testDirectory = describe "Print File Structure with a block comment section" $ do
  context "when the section contains empty and non-empty lines" $
    it "The printed file should match our sample comment file" $
      commentFile `shouldProduce` (testDirectory ++ commentTestFileName)

importTests :: FilePath -> Spec
importTests testDirectory = describe "Print File Structure with an imports section" $ do
  context "when the section contains file imports and module imports" $
    it "The printed file should match our sample imports file" $
      importMixedFile `shouldProduce` (testDirectory ++ importMixedTestFileName)

  context "when the section contains a single file import" $
    it "The printed file should match the single import file" $
      importHeaderFile `shouldProduce` (testDirectory ++  importHeaderTestFileName)

  context "when the section contains a single module import" $
    it "The printed file should match the single module import file" $
      importModuleFile `shouldProduce` (testDirectory ++  importModuleTestFileName)

interfaceTests :: FilePath -> Spec
interfaceTests testDirectory = describe "Print File Structure with an interface section" $ do
  context "when the section has no methods" $
    it "The printed file should match our no methods interface section" $
      noMethodInterfaceFile `shouldProduce` (testDirectory ++ noMethodInterfaceTestFileName)

  context "when the section has methods" $
    it "The printed file should match our regular interface section" $
      interfaceWithMethodsFile `shouldProduce` (testDirectory ++ interfaceWithMethodsTestFileName)

implementationTests :: FilePath -> Spec
implementationTests testDirectory = describe "Print File Structure with an implementation section" $ do
  context "when the section has no methods" $
    it "The printed file should match our no methods implementation section" $
      noMethodImplementationFile `shouldProduce` (testDirectory ++ noMethodImplementationTestFileName)

  context "when the section has methods" $
    it "The printed file should match our regular implementation section" $
      implementationWithMethodsFile `shouldProduce` (testDirectory ++ implementationWithMethodsTestFileName)

integrationTests :: FilePath -> Spec
integrationTests testDirectory = describe "Print File Structure with comments, imports, and a category section" $ do
  context "when we have mixed imports and an interface section" $
    it "The printed file should match our sample interface file" $
      fullInterfaceFile `shouldProduce` (testDirectory ++ fullInterfaceTestFileName)

  context "when we have a file import and an implementation section" $
    it "The printed file should match our sample implementation file" $
      fullImplementationFile `shouldProduce` (testDirectory ++ fullImplementationTestFileName)

shouldProduce :: ObjcFile -> FilePath -> Expectation
shouldProduce objcFile filename = do
  let actualPath = filename ++ resultExtension
  printStructureToFile objcFile actualPath
  actualFileString <- readFile actualPath
  expectedFileString <- readFile (filename ++ testExtension)
  actualFileString `shouldBe` expectedFileString

commentFile :: ObjcFile
commentFile = ObjcFile []

importMixedFile :: ObjcFile
importMixedFile = ObjcFile []

importHeaderFile :: ObjcFile
importHeaderFile = ObjcFile []

importModuleFile :: ObjcFile
importModuleFile = ObjcFile []

noMethodInterfaceFile :: ObjcFile
noMethodInterfaceFile = ObjcFile []

interfaceWithMethodsFile :: ObjcFile
interfaceWithMethodsFile = ObjcFile []

noMethodImplementationFile :: ObjcFile
noMethodImplementationFile = ObjcFile []

implementationWithMethodsFile :: ObjcFile
implementationWithMethodsFile = ObjcFile []

fullInterfaceFile :: ObjcFile
fullInterfaceFile = ObjcFile []

fullImplementationFile :: ObjcFile
fullImplementationFile = ObjcFile []

commentTestFileName :: FilePath
commentTestFileName = "comment"

importMixedTestFileName :: FilePath
importMixedTestFileName = "importMixed"

importHeaderTestFileName :: FilePath
importHeaderTestFileName = "importHeader"

importModuleTestFileName :: FilePath
importModuleTestFileName = "importModule"

noMethodInterfaceTestFileName :: FilePath
noMethodInterfaceTestFileName = "noMethodInterface"

interfaceWithMethodsTestFileName :: FilePath
interfaceWithMethodsTestFileName = "interfaceWithMethods"

noMethodImplementationTestFileName :: FilePath
noMethodImplementationTestFileName = "noMethodImplementation"

implementationWithMethodsTestFileName :: FilePath
implementationWithMethodsTestFileName = "implementationWithMethods"

fullInterfaceTestFileName :: FilePath
fullInterfaceTestFileName = "fullInterface"

fullImplementationTestFileName :: FilePath
fullImplementationTestFileName = "fullImplementation"

testExtension :: String
testExtension = ".test"

resultExtension :: String
resultExtension = ".result"
