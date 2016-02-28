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
  "Created By James Bowen 2/28/2016",
  "Copyright (c) 2016 OneWeekApps. All Rights Reserved",
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

fontsImplementationBlockComment :: FileSection
fontsImplementationBlockComment = BlockCommentSection
  ["",
  "UIFont+MyAppFonts.m",
  "MySampleApp",
  "",
  "Created By James Bowen 2/28/2016",
  "Copyright (c) 2016 OneWeekApps. All Rights Reserved",
  ""]

fontsImplementationImports :: FileSection
fontsImplementationImports = ImportsSection
  [FileImport "UIFont+MyAppFonts.h"]

fontsImplementationSection :: FileSection
fontsImplementationSection = CategoryImplementationSection fontsCategory

fontsImplementationFile :: ObjcFile
fontsImplementationFile = ObjcFile
  [fontsImplementationBlockComment,
  fontsImplementationImports,
  fontsImplementationSection]

fontsCategory :: Category
fontsCategory = Category {
  originalTypeName = "UIFont",
  categoryName = "MyAppFonts",
  categoryMethods = map methodFromFont allTestFonts
}

methodFromFont :: OWAFont -> ObjcMethod
methodFromFont font = ObjcMethod {
  isStatic = True,
  nameIntro = fontName font,
  returnType = PointerType "UIFont",
  params = [],
  methodBody = [ReturnStatement $ MethodCall
                Var "UIFont"
                fontWithNameMethod
                [StringLit $ familyNameForFont font, FloatLit $ fontSize font]]
}

familyNameForFont :: OWAFont -> String
familyNameForFont font = case fontStyles font of
  [] -> fontFamilyName font
  styles -> fontFamilyName font ++ '-':styleList
    where styleList = foldl (\str style -> str ++ (show style)) styles

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
