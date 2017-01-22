module Parse.Tests.Alerts.Errors where

import Model.OWAParseError
import Text.Parsec.Pos

alertKeywordFailure1 :: SourcePos
alertKeywordFailure1 = newPos "alertKeywordFailure1.alerts" 1 1

alertKeywordFailure2 :: SourcePos
alertKeywordFailure2 = newPos "alertKeywordFailure2.alerts" 6 1

alertNameFailure1 :: SourcePos
alertNameFailure1 = newPos "alertNameFailure1.alerts" 1 7 

alertNameFailure2 :: SourcePos
alertNameFailure2 = newPos "alertNameFailure2.alerts" 7 7

badAttributeFailure1 :: SourcePos
badAttributeFailure1 = newPos "badAttributeFailure1.alerts" 5 3

badAttributeFailure2 :: SourcePos
badAttributeFailure2 = newPos "badAttributeFailure2.alerts" 3 3

badLocalizedKeyFailure1 :: SourcePos
badLocalizedKeyFailure1 = newPos "badLocalizedKeyFailure1.alerts" 2 9 

badLocalizedKeyFailure2 :: SourcePos
badLocalizedKeyFailure2 = newPos "badLocalizedKeyFailure2.alerts" 3 25

badButtonKeyFailure1 :: SourcePos
badButtonKeyFailure1 = newPos "badButtonKeyFailure1.alerts" 8 17

badButtonKeyFailure2 :: SourcePos
badButtonKeyFailure2 = newPos "badButtonKeyFailure2.alerts" 4 17

badButtonKeyFailure3 :: SourcePos
badButtonKeyFailure3 = newPos "badButtonKeyFailure3.alerts" 4 13

badButtonKeyFailure4 :: SourcePos
badButtonKeyFailure4 = newPos "badButtonKeyFailure4.alerts" 2 18

newLineEndFailure :: SourcePos
newLineEndFailure = newPos "newLineEndFailure.alerts" 5 16

allItemErrors :: [OWAParseError]
allItemErrors = 
  [ failureAlertError
  , underSpecYesNoError
  , noYesError ]

failureAlertError :: OWAParseError
failureAlertError = ObjectError {
  fileName = "itemFailures.alerts",
  itemName = "failureAlert",
  missingRequiredAttributes = ["Any Button Format"]
}

underSpecYesNoError :: OWAParseError
underSpecYesNoError = ObjectError {
  fileName = "itemFailures.alerts",
  itemName = "underSpecYesNo",
  missingRequiredAttributes = ["NoButton"]
}

noYesError :: OWAParseError
noYesError = ObjectError {
  fileName = "itemFailures.alerts",
  itemName = "noYes",
  missingRequiredAttributes = ["YesButton"]
}
