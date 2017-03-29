{-|
Module      : Swift.ModelConverter
Description : Module for Converting OWAModels to Swift objects
Copyright   : (c) James Bowen, 2017
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Swift.ModelConverter (
  swiftFileFromModel
) where

import           Model.OWAAppInfo
import           Model.OWAModel
import           Swift.AbSyn hiding (OptionalType, ArrayType)
import qualified Swift.AbSyn as A
import           Swift.Utils

--------------------------------------------------------------------------------
--------------------------ENTRY METHODS-----------------------------------------
--------------------------------------------------------------------------------

-- | 'swiftFileFromModel' takes the app info,
-- and a model object, and returns the structure for the model's
-- header file in Swift
swiftFileFromModel :: OWAAppInfo -> OWAModel -> SwiftFile
swiftFileFromModel appInfo model = SwiftFile 
  [ extensionCommentSection (typeName ++ ".swift") appInfo 
  , foundationImportSection 
  , modelClassSection ]
  where
    typeName = modelType model 
    modelClassSection = ClassSection typeName [originalObjectType]
      [ fieldsSection
      , initializerSection]
    fieldsSection = StatementListSection 
      (Just "Class Fields") 
      False
      (fieldStatement <$> modelFields model)
    initializerSection = MethodImplementationListSection (Just "Initializer") [initMethod model]

--------------------------------------------------------------------------------
--------------------------SWIFT HELPERS-----------------------------------------
--------------------------------------------------------------------------------

initMethod :: OWAModel -> SwiftMethod
initMethod model = SwiftMethod
  { isInitializer = True
  , qualifiers = []
  , name = "init"
  , returnType = Nothing
  , params = paramForField <$> modelFields model
  , methodBody = assignStatement <$> modelFields model }

paramForField :: OWAModelField -> ParamDef
paramForField field = ParamDef
  { paramLabelName = Just name
  , paramTitle = name
  , paramType = resolveType (fieldType field) }
  where
    name = fieldName field ++ "_"

resolveType :: OWAModelFieldType -> SwiftType
resolveType IntField = SimpleType "Int"
resolveType FloatField = SimpleType "Double"
resolveType BoolField = SimpleType "Bool"
resolveType StringField = SimpleType "String"
resolveType (CustomField name) = SimpleType name
resolveType (OptionalType typ) = A.OptionalType (resolveType typ)
resolveType (ArrayType typ) = A.ArrayType (resolveType typ)
resolveType (MapType typ) = DictionaryType (SimpleType "String") (resolveType typ)

assignStatement :: OWAModelField -> SwiftStatement
assignStatement field = AssignStatement
  (PropertyCall SelfExpr name)
  (Var (name ++ "_"))
  where
    name = fieldName field

fieldStatement :: OWAModelField -> SwiftStatement
fieldStatement field = VarDecl 
  qualifiers 
  (fieldName field) 
  (resolveType (fieldType field) )
  Nothing
  where
    qualifiers = if fieldReadOnly field
      then ["private(set)", "internal"]
      else ["internal"]

--------------------------------------------------------------------------------
--------------------------KEYWORDS----------------------------------------------
--------------------------------------------------------------------------------

originalObjectType :: String
originalObjectType = "NSObject"
