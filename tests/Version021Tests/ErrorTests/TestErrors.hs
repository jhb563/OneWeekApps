module TestErrors where

import OWAParseError
import Text.Parsec.Pos

customError1 :: SourcePos
customError1 = newPos "customViewError1.view" 4 5

customError2 :: [OWAParseError]
customError2 = [ObjectError {
  fileName = "customViewError2.view",
  itemName = "myCustomView",
  missingRequiredAttributes = ["Type"]
}]

customError3 :: SourcePos
customError3 = newPos "customViewError3.view" 4 5

customError4 :: SourcePos
customError4 = newPos "customViewError4.view" 6 7

containerError1 :: SourcePos
containerError1 = newPos "containerViewError1.view" 4 5

containerError2 :: SourcePos
containerError2 = newPos "containerViewError2.view" 4 5

containerError3 :: SourcePos
containerError3 = newPos "containerViewError3.view" 4 7

scrollError1 :: [OWAParseError]
scrollError1 = [ObjectError {
  fileName = "scrollViewError1.view",
  itemName = "myScrollView",
  missingRequiredAttributes = ["RightConstraint"]
}]

scrollError2 :: [OWAParseError]
scrollError2 = [ObjectError {
  fileName = "scrollViewError2.view",
  itemName = "myScrollView",
  missingRequiredAttributes = ["LeftConstraint", "RightConstraint"]
}]

scrollError3 :: [OWAParseError]
scrollError3 = [ObjectError {
  fileName = "scrollViewError3.view",
  itemName = "myScrollView",
  missingRequiredAttributes = ["TopConstraint", "BottomConstraint"]
}]

scrollError4 :: SourcePos
scrollError4 = newPos "scrollViewError4.view" 4 23
