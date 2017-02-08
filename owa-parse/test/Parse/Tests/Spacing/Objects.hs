module Parse.Tests.Spacing.Objects where

import Text.Parsec.Pos

emptyIndentColors :: SourcePos
emptyIndentColors = newPos "emptyIndentColors.colors" 2 1

emptyIndentFonts :: SourcePos
emptyIndentFonts = newPos "emptyIndentFonts.fonts" 2 1 

emptyIndentAlerts :: SourcePos
emptyIndentAlerts = newPos "emptyIndentAlerts.alerts" 2 1

emptyIndentErrors1 :: SourcePos
emptyIndentErrors1 = newPos "emptyIndentErrors1.errors" 2 1

emptyIndentErrors2 :: SourcePos
emptyIndentErrors2 = newPos "emptyIndentErrors2.errors" 2 1

changingIndentColors :: SourcePos
changingIndentColors = newPos "changingIndentColors.colors" 3 5

changingIndentFonts :: SourcePos
changingIndentFonts = newPos "changingIndentFonts.fonts" 3 3 

changingIndentAlerts :: SourcePos
changingIndentAlerts = newPos "changingIndentAlerts.alerts" 3 3

changingIndentErrors :: SourcePos
changingIndentErrors = newPos "changingIndentErrors.errors" 3 3 
