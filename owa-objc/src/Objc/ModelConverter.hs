{-|
Module      : Objc.ModelConverter
Description : Module for Converting OWAModels to Objective C objects
Copyright   : (c) James Bowen, 2017
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Objc.ModelConverter (
  objcHeaderFromModel,
  objcImplementationFromModel
) where

import           Data.Char (toUpper)
import           Data.List (sort)
import           Data.Maybe (catMaybes)

import           Model.OWAAppInfo
import           Model.OWAModel
import           Objc.AbSyn
import           Objc.Utils

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'objcHeaderFromModels' takes the app info,
-- and a model object, and returns the structure for the model's
-- header file in Objective C
objcHeaderFromModel :: OWAAppInfo -> OWAModel -> ObjcFile
objcHeaderFromModel appInfo model = ObjcFile $
  [ topCommentSection (mTy ++ ".h") appInfo
  , foundationImportsSection ] ++
  rest
  where
    mTy = modelType model
    properties = propertyForField True <$> modelFields model
    interfaceSection = InterfaceSection mTy (Just "NSObject") Nothing [] properties 
      [MethodHeaderListSection Nothing [initMethod model]]
    rest = case forwardClassSection model of
      Nothing -> [interfaceSection]
      Just declSection -> [declSection, interfaceSection]

-- | 'objcImplementationFromModels' takes the app info,
-- and a model object, and returns the structure for the model's
-- implementation file in Objective C
objcImplementationFromModel :: OWAAppInfo -> OWAModel -> ObjcFile
objcImplementationFromModel appInfo model = ObjcFile $
  [ topCommentSection (mTy ++ ".m") appInfo 
  , importsSection model ]
  ++ rest
  where
    mTy = modelType model
    implSection = ImplementationSection mTy Nothing [constructorSection]
    constructorSection = MethodImplementationListSection (Just "Constructor") [initMethod model]
    rest = case subInterfaceSection mTy (modelFields model) of
      Nothing -> [implSection]
      Just section -> [section, implSection]

--------------------------------------------------------------------------------
--------------------------SECTION HELPERS---------------------------------------
--------------------------------------------------------------------------------

forwardClassSection :: OWAModel -> Maybe FileSection
forwardClassSection model = case customClassesForModel model of
  [] -> Nothing
  classes -> Just $ ForwardDeclarationSection $ ClassDecl <$> sort classes

importsSection :: OWAModel -> FileSection
importsSection model = ImportsSection $
  importForType (modelType model) :
  map importForType (sort . customClassesForModel $ model)
  where 
    importForType typ = FileImport $ typ ++ ".h"

customClassesForModel :: OWAModel -> [String]
customClassesForModel model = foldl addCustomClass [] (fieldType <$> modelFields model)
  where
    addCustomClass :: [String] -> OWAModelFieldType -> [String]
    addCustomClass cs (CustomField customType) = customType : cs
    addCustomClass cs (OptionalType typ) = addCustomClass cs typ
    addCustomClass cs _ = cs

subInterfaceSection :: String -> [OWAModelField] -> Maybe FileSection
subInterfaceSection mTy fields = if null readOnlyProps
  then Nothing
  else Just $ InterfaceSection mTy Nothing Nothing [] fieldDefs []
  where
    readOnlyProps = filter fieldReadOnly fields
    fieldDefs = propertyForField False <$> readOnlyProps

--------------------------------------------------------------------------------
--------------------------OBJC HELPERS------------------------------------------
--------------------------------------------------------------------------------

propertyForField :: Bool -> OWAModelField -> ObjcProperty
propertyForField isTopInterface field = ObjcProperty
  { propertyType = resolveType typ
  , propertyAttributes = attrsForType (isTopInterface && fieldReadOnly field) typ
  , propertyName = fieldName field }
  where
    typ = fieldType field 

resolveType :: OWAModelFieldType -> ObjcType
resolveType IntField = SimpleType "int"
resolveType FloatField = SimpleType "double"
resolveType BoolField = SimpleType "BOOL"
resolveType StringField = PointerType "NSString"
resolveType (CustomField typeName) = PointerType typeName
resolveType (ArrayType _) = PointerType "NSArray"
resolveType (MapType _) = PointerType "NSDictionary"
resolveType (OptionalType optType) = case optType of
  IntField -> PointerType "int"
  FloatField -> PointerType "double"
  BoolField -> PointerType "BOOL"
  t -> resolveType t

attrsForType :: Bool -> OWAModelFieldType -> [String]
attrsForType isReadonly typ = catMaybes
  [ Just "nonatomic"
  , if isStrong typ then Just "strong" else Nothing
  , if isReadonly then Just "readonly" else Nothing ]

isStrong :: OWAModelFieldType -> Bool
isStrong (ArrayType _) = True
isStrong (MapType _) = True
isStrong (CustomField _) = True
isStrong StringField = True
isStrong (OptionalType typ') = isStrong typ'
-- TODO Are Maybes always Strong?
isStrong _ = False 

initMethod :: OWAModel -> ObjcMethod
initMethod model = ObjcMethod
  { isStatic = False
  , nameIntro = "initWith"
  , returnType = SimpleType "instancetype"
  , params = allParams
  , methodBody = initBody model }
  where
    allParams = capitalizeHead $ paramForField <$> modelFields model
    paramForField :: OWAModelField -> ParamDef
    paramForField field = ParamDef
      { paramTitle = fieldName field
      , paramType = resolveType (fieldType field)
      , paramName = fieldName field ++ "_"}

capitalizeHead :: [ParamDef] -> [ParamDef]
capitalizeHead [] = []
capitalizeHead (firstParam : restParams) = modifiedFirst : restParams
  where
    modifiedFirst = firstParam { paramTitle = capitalizeName (paramTitle firstParam) }
    capitalizeName "" = ""
    capitalizeName (a : as) = toUpper a : as

initBody :: OWAModel -> [ObjcStatement]
initBody model = 
  [ superStatement
  , ifBlock
  , ReturnStatement SelfExpr]
  where
    superStatement = AssignStatement SelfExpr $ MethodCall (Var "super") libInit []
    ifBlock = IfBlock SelfExpr (assignStatement <$> modelFields model)
    assignStatement field = AssignStatement
      (PropertyCall SelfExpr (fieldName field))
      (Var $ fieldName field ++ "_")

libInit :: CalledMethod
libInit = LibMethod {
  libNameIntro = "init",
  libParams = []
}
