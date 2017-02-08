module Parse.Tests.Views.Errors where

import Text.Parsec.Pos

import Model.OWAParseError

testLabelErrors :: [OWAParseError]
testLabelErrors = [ObjectError {
  fileName = "LabelItemError.view",
  itemName = "myLabel",
  missingRequiredAttributes = ["Text"]
}]

testButtonErrors :: [OWAParseError]
testButtonErrors = [ObjectError {
  fileName = "ButtonItemError.view",
  itemName = "myButton",
  missingRequiredAttributes = ["Text or ImageSrc"]
}]

testImageErrors :: [OWAParseError]
testImageErrors = [ObjectError {
  fileName = "ImageItemError.view",
  itemName = "newImage",
  missingRequiredAttributes = ["ImageSrc"]
}]

testCombinedErrors :: [OWAParseError]
testCombinedErrors = [ObjectError {
  fileName = "CombinedError.view",
  itemName = "b1",
  missingRequiredAttributes = ["Text or ImageSrc"]
}, ObjectError {
  fileName = "CombinedError.view",
  itemName = "l1",
  missingRequiredAttributes = ["Text"]
}]

uppercaseViewNameFailError :: SourcePos
uppercaseViewNameFailError = newPos "uppercaseViewNameFail.view" 1 6

lowercaseViewTagFailError :: SourcePos
lowercaseViewTagFailError = newPos "lowercaseViewTagFail.view" 1 1

invalidViewTagError :: SourcePos
invalidViewTagError = newPos "invalidViewTag.view" 6 3

invalidElementsTagError :: SourcePos
invalidElementsTagError = newPos "invalidElementsTag.view" 7 5

uppercaseElementNameFailError :: SourcePos
uppercaseElementNameFailError = newPos "uppercaseElementNameFail.view" 3 11

placeholderLabelFailError :: SourcePos
placeholderLabelFailError = newPos "placeholderLabelFail.view" 5 7

noIndentLayoutError :: SourcePos
noIndentLayoutError = newPos "noIndentLayout.view" 6 7

invalidLayoutTagError :: SourcePos
invalidLayoutTagError = newPos "invalidLayoutTag.view" 6 9

lowercaseLayoutTagError :: SourcePos
lowercaseLayoutTagError = newPos "lowercaseLayoutTag.view" 6 9

emptyHeightTagError :: SourcePos
emptyHeightTagError = newPos "emptyHeightTag.view" 6 15

emptyAboveTagError :: SourcePos
emptyAboveTagError = newPos "emptyAboveTag.view" 7 14

noViewToRightError :: SourcePos
noViewToRightError = newPos "noViewToRight.view" 6 19

incorrectCenterError :: SourcePos
incorrectCenterError = newPos "incorrectCenter.view" 8 9

imageButtonMissingQuotesError :: SourcePos
imageButtonMissingQuotesError = newPos "imageButtonError1.view" 4 16 

imageButtonWrongTagNameError :: SourcePos
imageButtonWrongTagNameError = newPos "imageButtonError2.view" 4 7 

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
