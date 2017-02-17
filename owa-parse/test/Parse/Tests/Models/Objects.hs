module Parse.Tests.Models.Objects where

import Model.OWAModel

basicModel :: OWAModel
basicModel = OWAModel
  { modelType = "MyFirstModel"
  , modelFields = 
    [ mkField "balance" FloatField True
    , mkField "firstString1" StringField True
    , mkField "flt" FloatField False
    , mkField "isGood" BoolField False
    , mkField "my1Value" IntField False
    , mkField "sEc0nd4ryValue3" IntField True
    , mkField "secondString2" StringField False ] }

customModel :: OWAModel
customModel = OWAModel
  { modelType = "CustomReferenceModel"
  , modelFields =
    [ mkField "myFloat" FloatField False
    , mkField "myModel" (CustomField "FirstModel") True
    , mkField "myModel2" (CustomField "SecondModel") False
    , mkField "name" StringField False ] }

optionalsModel :: OWAModel
optionalsModel = OWAModel
  { modelType = "OptionalsModel"
  , modelFields =
    [ mkField "maybeBool" (OptionalType BoolField) False 
    , mkField "maybeCustom" (OptionalType (CustomField "FirstModel")) False
    , mkField "maybeFloat" (OptionalType FloatField) False
    , mkField "maybeInt" (OptionalType IntField) False
    , mkField "maybeString" (OptionalType StringField) False
    , mkField "nestedMaybe" (OptionalType (OptionalType StringField)) False ] }

arraysModel :: OWAModel
arraysModel = OWAModel
  { modelType = "ArrayObject"
  , modelFields =
    [ mkField "arrayMaybes" (ArrayType (OptionalType StringField)) False
    , mkField "customs" (ArrayType (CustomField "SecondModel")) False
    , mkField "maybeArray" (OptionalType (ArrayType FloatField)) False
    , mkField "maybeBool" (OptionalType BoolField) False 
    , mkField "myIntArray" (ArrayType IntField) False
    , mkField "nestedMaybe" (OptionalType (OptionalType StringField)) False ] }

mapsModel :: OWAModel
mapsModel = OWAModel
  { modelType = "MapObject"
  , modelFields =
    [ mkField "customs" (MapType (CustomField "ThirdModel")) False
    , mkField "phoneNumbers" (MapType StringField) False
    , mkField "scores" (MapType IntField) False ] }

completeModel :: OWAModel
completeModel = OWAModel
  { modelType = "MyCompleteModel"
  , modelFields =
    [ mkField "addresses" (MapType StringField) True
    , mkField "crazyNested" ((MapType . ArrayType . OptionalType . MapType . CustomField) "FirstModel") False
    , mkField "flag" (OptionalType BoolField) True 
    , mkField "normalField1" IntField True
    , mkField "normalField2" StringField False
    , mkField "normalField3" FloatField False
    , mkField "user" (CustomField "User") False ] }

mkField :: String -> OWAModelFieldType -> Bool -> OWAModelField
mkField name typ readOnly = OWAModelField
  { fieldName = name
  , fieldType = typ
  , fieldReadOnly = readOnly }
