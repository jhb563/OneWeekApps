module TestFontObjcObjects where

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
  categoryMethods = []
}
