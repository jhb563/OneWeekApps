module TestDefaultErrors where

import OWAParseError
import Text.Parsec.Pos

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

