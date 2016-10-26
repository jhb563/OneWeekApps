{-|
Module      : OWASwiftAbSyn
Description : Module for Swift abstract syntax
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWASwiftAbSyn where

-- | 'Identifier' is a typedef for String, signaling that we are using
-- a variable name in Swift
type Identifier = String

-- | 'SwiftFile' is a structure for representing all the code and comments
-- in a Swift file. It currently stores a list of "sections" of
-- the file.
data SwiftFile = SwiftFile [FileSection] deriving (Show, Eq)

-- | 'FileSection' represents the different section of a Swift
-- file. Current, we allow for commented sections and import sections,
-- as well as extension sections and method implementation sections.
data FileSection =
  BlockCommentSection [String] |
  ImportsSection [Import] |
  ExtensionSection String [FileSection] |
  ClassSection Identifier Identifier [FileSection] |
  MethodImplementationListSection (Maybe String) [SwiftMethod] |
  StatementListSection (Maybe String) [SwiftStatement]
  deriving (Show, Eq)

-- | 'Import' represents an import statement, typically at the top of a
-- Swift file. For now we only allow library imports.
data Import =
  ModuleImport String
  deriving (Show, Eq)

-- | 'SwiftMethod' stores the structure of a particular method
-- implemented in our code.
data SwiftMethod = SwiftMethod {
  isInitializer :: Bool,
  qualifiers :: [String],
  name :: String,
  returnType :: Maybe SwiftType,
  params :: [ParamDef],
  methodBody :: [SwiftStatement]
} deriving (Show, Eq)

-- | 'CalledMethod' stores either a SwiftMethod which we have defined,
-- or a Libary method whose implementation is unknown.
data CalledMethod = UserMethod SwiftMethod |
  LibMethod {
    libMethodName :: String,
    libParams :: [Maybe String]
  } deriving (Show, Eq)

-- | 'Closure' represents a block which can be called with certain 
-- arguments. An anonymous function of sorts.
data Closure = Closure {
  closureParams :: [ParamDef],
  closureBody :: [SwiftStatement]
} deriving (Show, Eq)

-- | 'SwiftType' combines the possible types of types we can have in Swift.
data SwiftType = SimpleType String |
  OptionalType String |
  ExplicitType String |
  FunctionType [SwiftType] SwiftType
  deriving (Show, Eq)

-- | 'ParamDef' abstracts the three parts describing a method parameter
-- in Swift. It contains a title, type, and a name.
data ParamDef = ParamDef {
  paramLabelName :: Maybe String,
  paramTitle :: String,
  paramType :: SwiftType
} deriving (Show, Eq)

-- | 'SwiftStatement' is a signel unit of a Swift method implementation.
data SwiftStatement =
  ReturnStatement SwiftExpression |
  ExpressionStatement SwiftExpression |
  LetDecl Identifier SwiftExpression |
  VarDecl [String] Identifier SwiftType SwiftExpression |
  TypeAliasDecl Identifier SwiftType |
  AssignStatement SwiftExpression SwiftExpression |
  ForEachBlock SwiftExpression SwiftExpression [SwiftStatement]
  deriving (Show, Eq)

-- | 'SwiftExpression' represents an expression within Swift syntax.
-- These can vary from literal values to method calls and 
-- mathematical expressions.
data SwiftExpression =
  MethodCall (Maybe SwiftExpression) CalledMethod [SwiftExpression] |
  PropertyCall SwiftExpression Identifier |
  ClosureExpr Closure |
  CalledClosure Closure [SwiftExpression] |
  Var Identifier |
  FloatLit Float |
  StringLit String |
  BoolLit Bool |
  ArrayLit [SwiftExpression] |
  DictionaryLit [(SwiftExpression, SwiftExpression)]
  deriving (Show, Eq)
