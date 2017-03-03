module Parse.Tests.Models.Errors where

import Text.Parse.Pos

import Model.OWAParseError

lowercaseModelTagError :: SourcePos
lowercaseModelTagError = newPos "parseFail1.model" 1 1

lowercaseTypeTagError :: SourcePos
lowercaseTypeTagError = newPos "parseFail2.model" 2 3

lowercaseFieldTagError :: SourcePos
lowercaseFieldTagError = newPos "parseFail3.model" 5 3

uppercaseFieldNameError :: SourcePos
uppercaseFieldNameError = newPos "parseFail4.model" 2 9

lowercaseArrayTagError :: SourcePos
lowercaseArrayTagError = newPos "parseFail5.model" 4 10

lowercaseReadonlyTagError :: SourcePos
lowercaseReadonlyTagError = newPos "parseFail6.model" 4 5

lowercaseClassNameTagError :: SourcePos
lowercaseClassNameTagError = newPos "parseFail7.model" 3 10

lowercaseClassNameTagError :: SourcePos
lowercaseClassNameTagError = newPos "parseFail7.model" 3 10

readWriteError :: SourcePos
readWriteError = newPos "parseFail8.model" 5 5

doubleError :: SourcePos
doubleError = newPos "parseFail9.model" 4 10

noMaybeTypeError :: SourcePos
noMaybeTypeError = newPos "parseFail10.model" 3 15

noArrayTypeError :: SourcePos
noArrayTypeError = newPos "parseFail11.model" 3 15

noMapTypeError :: SourcePos
noMapTypeError = newPos "parseFail12.model" 3 13

noFieldName :: SourcePos
noFieldName = newPos "parseFail13.model" 2 8

noTypeForFieldError :: [OWAParseError]
noTypeForFieldError = [ObjectError {
  fileName = "parseFail14.model",
  itemName = "intField",
  missingRequiredAttributes = ["Type"]
}]
