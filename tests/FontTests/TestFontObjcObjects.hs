module TestFontObjcObjects where

import OWAFont
import OWAObjcAbSyn
import TestFonts

fontsHeaderBlockComment :: FileSection
fontsHeaderBlockComment = BlockCommentSection
  ["",
  "UIFont+MyAppFonts.h",
  "MySampleApp",
  "",
  "Created By James Bowen 2/16/2016",
  "Copyright (c) 2016 One Week Apps. All Rights Reserved",
  ""]

emptyFontsHeaderBlockComment :: FileSection
emptyFontsHeaderBlockComment = BlockCommentSection
  ["",
  "UIFont+EmptyCategory.h",
  "MySampleApp",
  "",
  "Created By James Bowen 2/16/2016",
  "Copyright (c) 2016 One Week Apps. All Rights Reserved",
  ""]

fontsHeaderImports :: FileSection
fontsHeaderImports = ImportsSection
  [ModuleImport "UIKit"]

fontsHeaderInterfaceSection :: FileSection
fontsHeaderInterfaceSection = CategoryInterfaceSection fontsCategory

fontsHeaderFile :: ObjcFile
fontsHeaderFile = ObjcFile 
  [fontsHeaderBlockComment,
  fontsHeaderImports,
  fontsHeaderInterfaceSection]

emptyFontsHeaderFile :: ObjcFile
emptyFontsHeaderFile = ObjcFile
  [emptyFontsHeaderBlockComment,
  fontsHeaderImports,
  CategoryInterfaceSection emptyFontsCategory]

fontsImplementationBlockComment :: FileSection
fontsImplementationBlockComment = BlockCommentSection
  ["",
  "UIFont+MyAppFonts.m",
  "MySampleApp",
  "",
  "Created By James Bowen 2/16/2016",
  "Copyright (c) 2016 One Week Apps. All Rights Reserved",
  ""]

emptyFontsImplementationBlockComment :: FileSection
emptyFontsImplementationBlockComment = BlockCommentSection
  ["",
  "UIFont+EmptyCategory.m",
  "MySampleApp",
  "",
  "Created By James Bowen 2/16/2016",
  "Copyright (c) 2016 One Week Apps. All Rights Reserved",
  ""]

fontsImplementationImports :: FileSection
fontsImplementationImports = ImportsSection
  [FileImport "UIFont+MyAppFonts.h"]

emptyFontsImplementationImports :: FileSection
emptyFontsImplementationImports = ImportsSection
  [FileImport "UIFont+EmptyCategory.h"]

fontsImplementationSection :: FileSection
fontsImplementationSection = CategoryImplementationSection fontsCategory

fontsImplementationFile :: ObjcFile
fontsImplementationFile = ObjcFile
  [fontsImplementationBlockComment,
  fontsImplementationImports,
  fontsImplementationSection]

emptyFontsImplementationFile :: ObjcFile
emptyFontsImplementationFile = ObjcFile
  [emptyFontsImplementationBlockComment,
  emptyFontsImplementationImports,
  CategoryImplementationSection emptyFontsCategory]

emptyFontsCategory :: Category
emptyFontsCategory = Category {
  originalTypeName = "UIFont",
  categoryName = "EmptyCategory",
  categoryMethods = []
}

fontsCategory :: Category
fontsCategory = Category {
  originalTypeName = "UIFont",
  categoryName = "MyAppFonts",
  categoryMethods = map methodFromFont sortedTestFonts
}

methodFromFont :: OWAFont -> ObjcMethod
methodFromFont font = ObjcMethod {
  isStatic = True,
  nameIntro = fontName font,
  returnType = PointerType "UIFont",
  params = [],
  methodBody = [ReturnStatement $ MethodCall
                (Var "UIFont")
                fontWithNameMethod
                [StringLit $ familyNameForFont font, FloatLit $ fontSize font]]
}

familyNameForFont :: OWAFont -> String
familyNameForFont font = case fontStyles font of
  [] -> fontFamily font
  styles -> fontFamily font ++ ('-':styleList)
    where styleList = foldl (\str style -> str ++ show style) "" styles

fontWithNameMethod :: ObjcMethod
fontWithNameMethod = ObjcMethod {
  isStatic = True,
  nameIntro = "fontWith",
  returnType = PointerType "UIFont",
  params = [ParamDef {
    paramTitle = "Name",
    paramType = PointerType "NSString",
    paramName = "name"
  }, ParamDef {
    paramTitle = "size",
    paramType = SimpleType "CGFloat",
    paramName = "size"
  }],
  methodBody = []
}
