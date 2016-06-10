module TestErrors where

import OWAParseError
import Text.Parsec.Pos

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
