-- OWAObjcPrint will expose the method
-- printStructureToFile :: ObjcFile -> FilePath -> IO ()
-- which takes an objective C file structure and a filepath
-- and prints the file structure to the given file

module ObjcPrintTests (
  runObjcPrintTests
) where

import OWAColor
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
blockCommentTests testDirectory = describe "Print File Structure with a block comment section" $
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
commentFile = ObjcFile
  [BlockCommentSection
    ["",
    "UIColor+SampleCategory.m",
    "MySampleApp",
    "",
    "Created By James Bowen 2/16/2016",
    "Copyright (c) 2016 One Week Apps. All Rights Reserved",
    ""]
  ]

importMixedFile :: ObjcFile
importMixedFile = ObjcFile 
  [ImportsSection
    [ModuleImport "Foundation",
    FileImport "MySampleApp.h",
    ModuleImport "UIKit",
    FileImport "MyHeader.h"]
  ]

importHeaderFile :: ObjcFile
importHeaderFile = ObjcFile
  [ImportsSection
    [FileImport "MySampleApp.h"]
  ]

importModuleFile :: ObjcFile
importModuleFile = ObjcFile
  [ImportsSection
    [ModuleImport "UIKit"]
  ]

noMethodInterfaceFile :: ObjcFile
noMethodInterfaceFile = ObjcFile [CategoryInterfaceSection emptyCategory]

interfaceWithMethodsFile :: ObjcFile
interfaceWithMethodsFile = ObjcFile [CategoryInterfaceSection categoryWithMethods]

noMethodImplementationFile :: ObjcFile
noMethodImplementationFile = ObjcFile [CategoryImplementationSection emptyCategory]

implementationWithMethodsFile :: ObjcFile
implementationWithMethodsFile = ObjcFile [CategoryImplementationSection categoryWithMethods]

fullInterfaceFile :: ObjcFile
fullInterfaceFile = ObjcFile 
  [BlockCommentSection 
    ["",
    "UIColor+SampleCategory.h",
    "MySampleApp",
    "",
    "Created By James Bowen 2/16/2016",
    "Copyright (c) 2016 One Week Apps. All Rights Reserved",
    ""],
  ImportsSection [ModuleImport "UIKit"],
  CategoryInterfaceSection categoryWithMethods]

fullImplementationFile :: ObjcFile
fullImplementationFile = ObjcFile 
  [BlockCommentSection 
    ["",
    "UIColor+SampleCategory.m",
    "MySampleApp",
    "",
    "Created By James Bowen 2/16/2016",
    "Copyright (c) 2016 One Week Apps. All Rights Reserved",
    ""],
  ImportsSection [FileImport "UIColor+SampleCategory.h"],
  CategoryImplementationSection categoryWithMethods]

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

emptyCategory :: Category
emptyCategory = Category {
  originalTypeName = "UIColor",
  categoryName = "NoMethodCategory",
  categoryMethods = []
}

categoryWithMethods :: Category
categoryWithMethods = Category {
  originalTypeName = "UIColor",
  categoryName = "SampleCategory",
  categoryMethods = [
    ObjcMethod {
      isStatic = True,
      nameIntro = "christmasColor",
      returnType = PointerType "UIColor",
      params = [],
      methodBody = [ReturnStatement $ methodBodyExprForColor christmasColor]
    },
    ObjcMethod {
      isStatic = True,
      nameIntro = "darkRed",
      returnType = PointerType "UIColor",
      params = [],
      methodBody = [ReturnStatement $ methodBodyExprForColor darkRedColor]
    },
    ObjcMethod {
      isStatic = True,
      nameIntro = "offWhite",
      returnType = PointerType "UIColor",
      params = [],
      methodBody = [ReturnStatement $ methodBodyExprForColor offWhiteColor]
    }
  ]
}

christmasColor :: OWAColor
christmasColor = OWAColor {
  colorName = "christmasColor",
  red = 0.9,
  green = 0.9,
  blue = 0.1,
  alpha = 0.5
}

darkRedColor :: OWAColor
darkRedColor = OWAColor {
  colorName = "darkRed",
  red = 0.4,
  green = 0.0,
  blue = 0.0,
  alpha = 1.0
}

offWhiteColor :: OWAColor
offWhiteColor = OWAColor {
  colorName = "offWhite",
  red = 0.9,
  green = 0.9,
  blue = 0.9,
  alpha = 1.0
}

methodBodyExprForColor :: OWAColor -> ObjcExpression
methodBodyExprForColor color = MethodCall (Var "UIColor") colorWithRGBAMethod 
  [FloatLit $ red color,
  FloatLit $ green color,
  FloatLit $ blue color,
  FloatLit $ alpha color]

colorWithRGBAMethod :: ObjcMethod
colorWithRGBAMethod = ObjcMethod {
  isStatic = True,
  nameIntro = "colorWith",
  returnType = PointerType "UIColor",
  params = 
    [ParamDef {
      paramTitle = "Red",
      paramType = SimpleType "CGFloat",
      paramName = "red"
    }, 
    ParamDef {
      paramTitle = "green",
      paramType = SimpleType "CGFloat",
      paramName = "green"
    }, 
    ParamDef {
      paramTitle = "blue",
      paramType = SimpleType "CGFloat",
      paramName = "blue"
    }, 
    ParamDef {
      paramTitle = "alpha",
      paramType = SimpleType "CGFloat",
      paramName = "alpha"
    }],
  methodBody = []
}

testExtension :: String
testExtension = ".test"

resultExtension :: String
resultExtension = ".result"
