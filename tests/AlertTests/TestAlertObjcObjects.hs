module TestAlertObjcObjects where

import OWAAlert
import OWAObjcAbSyn
import TestAlerts

alertsHeaderBlockComment :: FileSection
alertsHeaderBlockComment = BlockCommentSection
  ["",
  "UIAlertController+MyAppAlerts.h",
  "MySampleApp",
  "",
  "Created By James Bowen 2/16/2016",
  "Copyright (c) 2016 One Week Apps. All Rights Reserved",
  ""]

emptyAlertsHeaderBlockComment :: FileSection
emptyAlertsHeaderBlockComment = BlockCommentSection
  ["",
  "UIAlertController+EmptyCategory.h",
  "MySampleApp",
  "",
  "Created By James Bowen 2/16/2016",
  "Copyright (c) 2016 One Week Apps. All Rights Reserved",
  ""]

alertsHeaderImports :: FileSection
alertsHeaderImports = ImportsSection [ModuleImport "UIKit"]

alertTypedefSection :: FileSection
alertTypedefSection = ForwardDeclarationSection [TypeDefDecl (SimpleType "void") "AlertHandler" []

alertsHeaderInterfaceSection :: FileSection
alertsHeaderInterfaceSection = CategoryInterfaceSection alertsCategory

alertsHeaderFile :: ObjcFile
alertsHeaderFile = [alertsHeaderBlockComment,
  alertsHeaderImports,
  alertTypedefSection,
  alertsHeaderInterfaceSection]

emptyAlertsHeaderFile :: ObjcFile
emptyAlertsHeaderFile = [emptyAlertsHeaderBlockComment,
  alertsHeaderImports,
  alertTypedefSection,
  CategoryInterfaceSection emptyAlertsCategory]

alertsImplementationBlockComment :: FileSection
alertsImplementationBlockComment = BlockCommentSection
  ["",
  "UIAlertController+MyAppAlerts.m",
  "MySampleApp",
  "",
  "Created By James Bowen 2/16/2016",
  "Copyright (c) 2016 One Week Apps. All Rights Reserved",
  ""]

emptyAlertsImplementationBlockComment :: FileSection
emptyAlertsImplementationBlockComment = BlockCommentSection
  ["",
  "UIAlertController+EmptyCategory.m",
  "MySampleApp",
  "",
  "Created By James Bowen 2/16/2016",
  "Copyright (c) 2016 One Week Apps. All Rights Reserved",
  ""]

alertsImplementationImports :: FileSection
alertsImplementationImports = ImportsSection [FileImport "UIAlertController+MyAppAlerts.h"]

emptyAlertsImplementationImports :: FileSection
emptyAlertsImplementationImports = ImportsSection [FileImport "UIAlertController+EmptyCategory.h"]

alertsImplementationSection :: FileSection
alertsImplementationSection = CategoryImplementationSection alertsCategory

alertsImplementationFile :: ObjcFile
alertsImplementationFile = ObjcFile
  [alertsImplementationBlockComment,
  alertsImplementationImports,
  alertsImplementationSection]

emptyAlertsImplementationFile :: ObjcFile
emptyAlertsImplementationFile = ObjcFile
  [emptyAlertsImplementationBlockComment,
  emptyAlertsImplementationImports,
  CategoryImplementationSection emptyAlertsCategory]

emptyAlertsCategory :: Category
emptyAlertsCategory = Category {
  originalTypeName = "UIAlertController",
  categoryName = "EmptyCategory",
  categoryMethods = []
}
 
alertsCategory :: Category
alertsCategory = Category {
  originalTypeName = "UIAlertController",
  categoryName = "MyAppAlerts",
  categoryMethods = map methodFromAlert sortedTestAlerts
}

methodFromAlert :: OWAAlert -> ObjcMethod

