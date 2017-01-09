module TestParseErrors where

import Model.OWAParseError

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
  fileName = "alertFailures.alerts",
  itemName = "failureAlert",
  missingRequiredAttributes = ["Any Button Format"]
}

underSpecYesNoError :: OWAParseError
underSpecYesNoError = ObjectError {
  fileName = "alertFailures.alerts",
  itemName = "underSpecYesNo",
  missingRequiredAttributes = ["NoButton"]
}

noYesError :: OWAParseError
noYesError = ObjectError {
  fileName = "alertFailures.alerts",
  itemName = "noYes",
  missingRequiredAttributes = ["YesButton"]
}

failColor1Error :: OWAParseError
failColor1Error = ObjectError {
  fileName = "colorFailures.colors",
  itemName = "failColor1",
  missingRequiredAttributes = ["Green"]
}

failColor2Error :: OWAParseError
failColor2Error = ObjectError {
  fileName = "colorFailures.colors",
  itemName = "failColor2",
  missingRequiredAttributes = ["Red"]
}

failColor3Error :: OWAParseError
failColor3Error = ObjectError {
  fileName = "colorFailures.colors",
  itemName = "failColor3",
  missingRequiredAttributes = ["Blue"]
}

missingCombo1Error :: OWAParseError
missingCombo1Error = ObjectError {
  fileName = "colorFailures.colors",
  itemName = "missingCombo1",
  missingRequiredAttributes = ["Blue", "Green"]
}

missingCombo2Error :: OWAParseError
missingCombo2Error = ObjectError {
  fileName = "colorFailures.colors",
  itemName = "missingCombo2",
  missingRequiredAttributes = ["Green", "Red"]
}

noDomainError :: OWAParseError
noDomainError = ObjectError {
  fileName = "errorFailures.errors",
  itemName = "noDomain",
  missingRequiredAttributes = ["Domain"]
}

justDescriptionError :: OWAParseError
justDescriptionError = ObjectError {
  fileName = "errorFailures.errors",
  itemName = "justDescription",
  missingRequiredAttributes = ["Code", "Domain"]
}

comboError :: OWAParseError
comboError = ObjectError {
  fileName = "errorFailures.errors",
  itemName = "combo",
  missingRequiredAttributes = ["Code", "Description"]
}

noCodeError :: OWAParseError
noCodeError = ObjectError {
  fileName = "errorFailures.errors",
  itemName = "noCode",
  missingRequiredAttributes = ["Code"]
}

noDescriptionError :: OWAParseError
noDescriptionError = ObjectError {
  fileName = "errorFailures.errors",
  itemName = "noDescription",
  missingRequiredAttributes = ["Description"]
}

missingFamilyError :: OWAParseError
missingFamilyError = ObjectError {
  fileName = "fontFailures.fonts",
  itemName = "missingFamily",
  missingRequiredAttributes = ["FontFamily"]
}

missingSizeError :: OWAParseError
missingSizeError = ObjectError {
  fileName = "fontFailures.fonts",
  itemName = "missingSize",
  missingRequiredAttributes = ["Size"]
}

missingComboError :: OWAParseError
missingComboError = ObjectError {
  fileName = "fontFailures.fonts",
  itemName = "missingCombo",
  missingRequiredAttributes = ["FontFamily", "Size"]
}

