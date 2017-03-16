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
objcHeaderFromModel appInfo model = ObjcFile
  [ topCommentSection (mTy ++ ".h") appInfo
  , foundationImportsSection
  , interfaceSection ]
  where
    mTy = modelType model
    properties = propertyForField <$> modelFields model
    interfaceSection = InterfaceSection mTy (Just "NSObject") Nothing properties 
      [MethodHeaderListSection Nothing [initMethod model]]

-- | 'objcImplementationFromModels' takes the app info,
-- and a model object, and returns the structure for the model's
-- implementation file in Objective C
objcImplementationFromModel :: OWAAppInfo -> OWAModel -> ObjcFile
objcImplementationFromModel _ _ = ObjcFile []

propertyForField :: OWAModelField -> ObjcProperty
propertyForField field = ObjcProperty
  { propertyType = resolveType typ
  , propertyAttributes = attrsForType (fieldReadOnly field)  typ
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
  , methodBody = [] }
  where
    allParams = capitalizeHead $ paramForField <$> (modelFields model)
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
