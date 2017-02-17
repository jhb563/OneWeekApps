{-|
Module      : OWAModel
Description : Module for model model. We will use these to create class objects.
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Model.OWAModel where

data OWAModel = OWAModel 
  { modelType  :: String
  , modelFields :: [OWAModelField] }
  deriving (Show, Eq)

data OWAModelField = OWAModelField
  { fieldName     :: String
  , fieldType     :: OWAModelFieldType
  , fieldReadOnly :: Bool }
  deriving (Show, Eq)

data OWAModelFieldType = 
  IntField |
  FloatField |
  BoolField |
  StringField |
  CustomField String |
  OptionalType OWAModelFieldType |
  ArrayType OWAModelFieldType |
  MapType OWAModelFieldType
  deriving (Show, Eq)
