{-|
Module      : OWAObjcAbSyn
Description : Module for Objective C abstract syntax
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAObjcAbSyn where

-- | 'Identifier' is a typedef for String, signaling that we are using
-- a variable name in Objective C
type Identifier = String

-- | 'ObjcFile' is a structure for representing all the code and comments
-- in an Objective C file. It currently stores a list of "sections" of
-- the file.
data ObjcFile = ObjcFile [FileSection] deriving (Show, Eq)

-- | 'FileSection' represents the different section of an Objective C
-- file. Current, we allow for commented sections and import sections,
-- as well as interfaces and implementations for categories. This will
-- expand in the future to include protocol definitions, lists of forward
-- declarations, interfaces and implementations for normal classes. 
data FileSection = 
  BlockCommentSection [String] |
  ImportsSection [Import] |
  CategoryInterfaceSection Category |
  CategoryImplementationSection Category 
  deriving (Show, Eq)

-- | 'Import' represents an import statement, typically at the top of an
-- Objective C file. It can either be a module that we are importing, or
-- a particular header file.
data Import = 
  ModuleImport String |
  FileImport String
  deriving (Show, Eq)

-- | 'Category' stores the structure of an Objective C class extension.
-- For now, this structure only allows extending the structure with
-- methods, and not properties.
data Category = Category {
  originalTypeName :: String,
  categoryName :: String,
  categoryMethods :: [ObjcMethod]
} deriving (Show, Eq)

-- | 'ObjcMethod' stores the structure of a particular method. Methods
-- for which the implementation is unimportant or unknown can use an empty
-- list for the methodBody.
data ObjcMethod = ObjcMethod {
  isStatic :: Bool,
  nameIntro :: String,
  returnType :: ObjcType,
  params :: [ParamDef],
  methodBody :: [ObjcStatement]
} deriving (Show, Eq)

-- | 'ObjcType' divides our possible types in Objective C as either "simple"
-- or "pointer" types. The most clear reason we want this distinction is to
-- avoid having to store asterisks as part of a type's name.
data ObjcType = 
  SimpleType String |
  PointerType String 
  deriving (Show, Eq)

-- | 'ParamDef' abstracts the three parts of describing a method parameter in
-- Objective C. It needs a title, type, and a name.
data ParamDef = ParamDef {
  paramTitle :: String,
  paramType :: ObjcType,
  paramName :: String
} deriving (Show, Eq)

-- | 'ObjcStatement' is a single unit of a method implementation. For now,
-- the only type of statement we need is a return statement, but there will
-- be more in the future. 
data ObjcStatement = ReturnStatement ObjcExpression deriving (Show, Eq)

-- | 'ObjcExpression' represents an expression within Objective C syntax. This
-- will ultimately include more complicated types of expressions. 
data ObjcExpression = 
  MethodCall ObjcExpression ObjcMethod [ObjcExpression] |
  Var Identifier |
  StringLit String |
  FloatLit Float
  deriving (Show, Eq)
