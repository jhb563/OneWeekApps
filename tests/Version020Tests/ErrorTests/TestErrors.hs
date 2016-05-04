module TestErrors where

import OWAParseError

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
  missingRequiredAttributes = ["Text"]
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
  missingRequiredAttributes = ["Text"]
}, ObjectError {
  fileName = "CombinedError.view",
  itemName = "l1",
  missingRequiredAttributes = ["Text"]
}]
