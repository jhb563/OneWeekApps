{-|
Module      : Objc.AbSyn
Description : Module for Objective C abstract syntax
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Objc.AbSyn where

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
  ForwardDeclarationSection [ForwardDeclaration] |
  InterfaceSection String (Maybe String) (Maybe String) [String] [ObjcProperty] [FileSection] |
  ImplementationSection String (Maybe String) [FileSection] |
  MethodHeaderListSection (Maybe String) [ObjcMethod] |
  MethodImplementationListSection (Maybe String) [ObjcMethod] |
  LocalizedStringListSection String [ObjcStatement] |
  CMainMethodSection
  deriving (Show, Eq)

-- | 'Import' represents an import statement, typically at the top of an
-- Objective C file. It can either be a module that we are importing, or
-- a particular header file.
data Import = 
  ModuleImport String |
  FileImport String
  deriving (Show, Eq)

-- | 'ForwardDeclaration' represents a statement declaring something before
-- the main body of a file, such as a block typedef, class, or protocol
data ForwardDeclaration =
  TypedefDecl ObjcType Identifier [ObjcType] |
  EnumDecl Identifier [Identifier] |
  ClassDecl Identifier
  deriving (Show, Eq)

-- | 'Category' stores the structure of an Objective C class extension.
-- For now, this structure only allows extending the structure with
-- methods, and not properties.
data Category = Category {
  originalTypeName :: String,
  categoryName :: String,
  categoryMethods :: [ObjcMethod]
} deriving (Show, Eq)

-- | 'ObjcProperty' stores the structure for a property declaration
data ObjcProperty = ObjcProperty {
  propertyType :: ObjcType,
  propertyAttributes :: [String],
  propertyName :: String
} deriving (Show, Eq)

-- | 'ObjcMethod' stores the structure of a particular method. 
data ObjcMethod = ObjcMethod {
  isStatic :: Bool,
  nameIntro :: String,
  returnType :: ObjcType,
  params :: [ParamDef],
  methodBody :: [ObjcStatement]
} deriving (Show, Eq)

-- | 'CalledMethod' stores either an ObjcMethod which we have defined,
-- or a Library method whose implementation is unimportant.
data CalledMethod = UserMethod ObjcMethod |
  LibMethod {
    libNameIntro :: String,
    libParams :: [String]
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

-- | 'BlockParam' is similar to ParamDef, but does not have a title
data BlockParam = BlockParam {
  blockParamType :: ObjcType,
  blockParamName :: String
} deriving (Show,Eq)

-- | 'ObjcStatement' is a single unit of a method implementation.
data ObjcStatement =
  ReturnStatement ObjcExpression |
  ExpressionStatement ObjcExpression |
  IfBlock ObjcExpression [ObjcStatement] |
  ForEachBlock ObjcExpression ObjcExpression [ObjcStatement] |
  AssignStatement ObjcExpression ObjcExpression
  deriving (Show, Eq)

-- | 'ObjcExpression' represents an expression within Objective C syntax. This
-- will ultimately include more complicated types of expressions. 
data ObjcExpression = 
  SelfExpr |
  MethodCall ObjcExpression CalledMethod [ObjcExpression] |
  CFunctionCall String [ObjcExpression] |
  BinOp ObjcExpression Operator ObjcExpression |
  PropertyCall ObjcExpression Identifier |
  VoidBlock [BlockParam] [ObjcStatement] |
  Var Identifier |
  VarDecl ObjcType Identifier |
  DictionaryLit [(ObjcExpression, ObjcExpression)] |
  StringLit String |
  CStringLit String |
  FloatLit Float |
  ArrayLit [ObjcExpression] |
  BoolLit Bool
  deriving (Show, Eq)

-- | 'Operator' represents operators such as +,-,*,= etc.
data Operator = 
  Assign
  deriving (Show, Eq)
