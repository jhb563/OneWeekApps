{-|
Module      : OWAObjcAbSyn
Description : Module for Objective C abstract syntax
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAObjcAbSyn where

type Identifier = String

data ObjcFile = ObjcFile [FileSection] 
                deriving (Show, Eq)

data FileSection = 
  BlockCommentSection [String] |
  ImportsSection [Import] |
  CategoryInterfaceSection Category |
  CategoryImplementationSection Category
  deriving (Show, Eq)

data Import = ModuleImport String |
              FileImport String
              deriving (Show, Eq)

data Category = Category {
  originalTypeName :: String,
  categoryName :: String,
  categoryMethods :: [ObjcMethod]
} deriving (Show, Eq)

data ObjcMethod = ObjcMethod {
  isStatic :: Bool,
  nameIntro :: String,
  returnType :: ObjcType,
  params :: [ParamDef],
  methodBody :: [ObjcStatement]
} deriving (Show, Eq)

data ObjcType = SimpleType String |
                PointerType String 
                deriving (Show, Eq)

data ParamDef = ParamDef {
  paramTitle :: String,
  paramType :: ObjcType,
  paramName :: String
} deriving (Show, Eq)

data ObjcStatement = ReturnStatement ObjcExpression
                     deriving (Show, Eq)

data ObjcExpression = MethodCall ObjcExpression ObjcMethod [ObjcExpression] |
                      Var Identifier |
                      FloatLit Float
                      deriving (Show, Eq)
