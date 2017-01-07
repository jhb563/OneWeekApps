module TestDefaultErrors where

import Text.Parsec.Pos

import OWAParseError

appError1 :: SourcePos
appError1 = newPos "defaultFailTest1.info" 1 1

appError2 :: SourcePos
appError2 = newPos "defaultFailTest2.info" 1 1

appError3 :: SourcePos
appError3 = newPos "defaultFailTest3.info" 1 8

appError4 :: SourcePos
appError4 = newPos "defaultFailTest4.info" 2 12 

appError5 :: OWAParseError
appError5  = ObjectError {
  fileName = "defaultFailTest5.info",
  itemName = "appInfo",
  missingRequiredAttributes = ["AppName"]
}

appError6 :: OWAParseError
appError6  = ObjectError {
  fileName = "defaultFailTest6.info",
  itemName = "appInfo",
  missingRequiredAttributes = ["Prefix"]
}

appError7 :: SourcePos
appError7 = newPos "defaultFailTest7.info" 2 11

appError8 :: SourcePos
appError8 = newPos "defaultFailTest8.info" 2 12

appError9 :: SourcePos
appError9 = newPos "defaultFailTest9.info" 2 9
