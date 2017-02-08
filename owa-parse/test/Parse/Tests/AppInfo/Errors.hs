module Parse.Tests.AppInfo.Errors where

import Text.Parsec.Pos

import Model.OWAParseError

appError1 :: SourcePos
appError1 = newPos "appInfoFailTest1.info" 1 1

appError2 :: SourcePos
appError2 = newPos "appInfoFailTest2.info" 1 1

appError3 :: SourcePos
appError3 = newPos "appInfoFailTest3.info" 1 8

appError4 :: SourcePos
appError4 = newPos "appInfoFailTest4.info" 2 12 

appError5 :: OWAParseError
appError5  = ObjectError {
  fileName = "appInfoFailTest5.info",
  itemName = "appInfo",
  missingRequiredAttributes = ["AppName"]
}

appError6 :: OWAParseError
appError6  = ObjectError {
  fileName = "appInfoFailTest6.info",
  itemName = "appInfo",
  missingRequiredAttributes = ["Prefix"]
}

appError7 :: SourcePos
appError7 = newPos "appInfoFailTest7.info" 2 11

appError8 :: SourcePos
appError8 = newPos "appInfoFailTest8.info" 2 12

appError9 :: SourcePos
appError9 = newPos "appInfoFailTest9.info" 2 9
