module Parse.Tests.Errors.Errors where

import Model.OWAParseError
import Text.Parsec.Pos

errorKeywordFailure1 :: SourcePos
errorKeywordFailure1 = newPos "errorKeywordFailure1.errors" 2 1

errorKeywordFailure2 :: SourcePos
errorKeywordFailure2 = newPos "errorKeywordFailure2.errors" 6 1

errorKeywordFailure3 :: SourcePos
errorKeywordFailure3 = newPos "errorKeywordFailure3.errors" 4 1

errorNameFailure1 :: SourcePos
errorNameFailure1 = newPos "errorNameFailure1.errors" 3 10 

errorNameFailure2 :: SourcePos
errorNameFailure2 = newPos "errorNameFailure2.errors" 1 21 

badAttributeFailure1 :: SourcePos
badAttributeFailure1 = newPos "badAttributeFailure1.errors" 7 3

badAttributeFailure2 :: SourcePos
badAttributeFailure2 = newPos "badAttributeFailure2.errors" 2 3

badAttributeFailure3 :: SourcePos
badAttributeFailure3 = newPos "badAttributeFailure3.errors" 3 3

badAttributeFailure4 :: SourcePos
badAttributeFailure4 = newPos "badAttributeFailure4.errors" 6 3

badCodeValueFailure :: SourcePos
badCodeValueFailure = newPos "badCodeValueFailure.errors" 8 11

badDescriptionValueFailure :: SourcePos
badDescriptionValueFailure = newPos "badDescriptionValueFailure.errors" 4 15

badDomainValueFailure :: SourcePos
badDomainValueFailure = newPos "badDomainValueFailure.errors" 2 12

badPrefixValueFailure :: SourcePos
badPrefixValueFailure = newPos "badPrefixValueFailure.errors" 2 14

newLineEndFailure :: SourcePos
newLineEndFailure = newPos "newLineEndFailure.errors" 4 25

allItemErrors :: [OWAParseError]
allItemErrors = 
  [ noDomainError
  , justDescriptionError
  , comboError
  , noCodeError
  , noDescriptionError ]

noDomainError :: OWAParseError
noDomainError = ObjectError {
  fileName = "itemFailures.errors",
  itemName = "noDomain",
  missingRequiredAttributes = ["Domain"]
}

justDescriptionError :: OWAParseError
justDescriptionError = ObjectError {
  fileName = "itemFailures.errors",
  itemName = "justDescription",
  missingRequiredAttributes = ["Code", "Domain"]
}

comboError :: OWAParseError
comboError = ObjectError {
  fileName = "itemFailures.errors",
  itemName = "combo",
  missingRequiredAttributes = ["Code", "Description"]
}

noCodeError :: OWAParseError
noCodeError = ObjectError {
  fileName = "itemFailures.errors",
  itemName = "noCode",
  missingRequiredAttributes = ["Code"]
}

noDescriptionError :: OWAParseError
noDescriptionError = ObjectError {
  fileName = "itemFailures.errors",
  itemName = "noDescription",
  missingRequiredAttributes = ["Description"]
}
