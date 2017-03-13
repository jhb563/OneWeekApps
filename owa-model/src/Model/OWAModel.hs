{-|
Module      : OWAModel
Description : Module for model model. We will use these to create class objects.
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Model.OWAModel where

-- | Model for a "model", representing a data class in the application.
data OWAModel = OWAModel 
  { modelType  :: String
  , modelFields :: [OWAModelField] }
  deriving (Show, Eq)

-- | Represents a particular field of a model. Needs a type, and a bool
-- for readonly or readwrite.
data OWAModelField = OWAModelField
  { fieldName     :: String
  , fieldType     :: OWAModelFieldType
  , fieldReadOnly :: Bool }
  deriving (Show, Eq)

instance Ord OWAModelField where
  f1 `compare` f2 = fieldName f1 `compare` fieldName f2

-- | Represents the different types our fields can have. Some of these are
-- compound types.
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
