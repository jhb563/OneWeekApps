module TestParseErrors where

import OWAParseError

allAlertErrors :: [OWAParseError]
allAlertErrors = [failureAlertError,
  underSpecYesNoError,
  noYesError]

allColorErrors :: [OWAParseError]
allColorErrors = [failColor1Error,
  failColor2Error,
  failColor3Error,
  missingCombo1Error,
  missingCombo2Error]

allErrorErrors :: [OWAParseError]
allErrorErrors = [noDomainError,
  justDescriptionError,
  comboError,
  noCodeError,
  noDescriptionError]

allFontErrors :: [OWAParseError]
allFontErrors = [missingFamilyError,
  missingSizeError,
  missingComboError]

failureAlertError :: OWAParseError
failureAlertError = ObjectError {
  itemName = "failureAlert",
  missingRequiredAttributes = ["Any Button Format"]
}

underSpecYesNoError :: OWAParseError
underSpecYesNoError = ObjectError {
  itemName = "underSpecYesNo",
  missingRequiredAttributes = ["NoButton"]
}

noYesError :: OWAParseError
noYesError = ObjectError {
  itemName = "noYes",
  missingRequiredAttributes = ["YesButton"]
}

failColor1Error :: OWAParseError
failColor1Error = ObjectError {
  itemName = "failColor1",
  missingRequiredAttributes = ["Green"]
}

failColor2Error :: OWAParseError
failColor2Error = ObjectError {
  itemName = "failColor2",
  missingRequiredAttributes = ["Red"]
}

failColor3Error :: OWAParseError
failColor3Error = ObjectError {
  itemName = "failColor3",
  missingRequiredAttributes = ["Blue"]
}

missingCombo1Error :: OWAParseError
missingCombo1Error = ObjectError {
  itemName = "missingCombo1",
  missingRequiredAttributes = ["Blue", "Green"]
}

missingCombo2Error :: OWAParseError
missingCombo2Error = ObjectError {
  itemName = "missingCombo2",
  missingRequiredAttributes = ["Green", "Red"]
}

noDomainError :: OWAParseError
noDomainError = ObjectError {
  itemName = "noDomain",
  missingRequiredAttributes = ["Domain"]
}

justDescriptionError :: OWAParseError
justDescriptionError = ObjectError {
  itemName = "justDescription",
  missingRequiredAttributes = ["Code", "Domain"]
}

comboError :: OWAParseError
comboError = ObjectError {
  itemName = "combo",
  missingRequiredAttributes = ["Code", "Description"]
}

noCodeError :: OWAParseError
noCodeError = ObjectError {
  itemName = "noCode",
  missingRequiredAttributes = ["Code"]
}

noDescriptionError :: OWAParseError
noDescriptionError = ObjectError {
  itemName = "noDescription",
  missingRequiredAttributes = ["Description"]
}

missingFamilyError :: OWAParseError
missingFamilyError = ObjectError {
  itemName = "missingFamily",
  missingRequiredAttributes = ["FontFamily"]
}

missingSizeError :: OWAParseError
missingSizeError = ObjectError {
  itemName = "missingSize",
  missingRequiredAttributes = ["Size"]
}

missingComboError :: OWAParseError
missingComboError = ObjectError {
  itemName = "missingCombo",
  missingRequiredAttributes = ["FontFamily", "Size"]
}

