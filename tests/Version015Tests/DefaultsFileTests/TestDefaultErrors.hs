module TestDefaultErrors where

import ErrorInfo
import OWAParseError

appError1 :: ErrorInfo
appError1 = "defaultFailTest1.info" 1 1

appError2 :: ErrorInfo
appError2 = "defaultFailTest2.info" 1 1

appError3 :: ErrorInfo
appError3 = "defaultFailTest3.info" 1 9

appError4 :: ErrorInfo
appError4 = "defaultFailTest4.info" 2 12 

appError5 :: OWAParseError
appError5  = ObjectError {
  fileName = "defaultFailTest5.info",
  itemName = "appInfo",
  missingRequiredAttributes = ["AppName"]
}

