module TestFontErrors where

import Text.Parsec.Pos

fontKeywordFailure1 :: SourcePos
fontKeywordFailure1 = newPos "fontKeywordFailure1.fonts" 1 1

fontKeywordFailure2 :: SourcePos
fontKeywordFailure2 = newPos "fontKeywordFailure2.fonts" 6 1

fontNameFailure1 :: SourcePos
fontNameFailure1 = newPos "fontNameFailure1.fonts" 1 10

fontNameFailure2 :: SourcePos
fontNameFailure2 = newPos "fontNameFailure2.fonts" 6 6

badAttributeFailure1 :: SourcePos
badAttributeFailure1 = newPos "badAttributeFailure1.fonts" 2 3

badAttributeFailure2 :: SourcePos
badAttributeFailure2 = newPos "badAttributeFailure2.fonts" 9 3

badFontFamilyFailure :: SourcePos
badFontFamilyFailure = newPos "badFontFamilyFailure.fonts" 2 17

badFontSizeFailure1 :: SourcePos
badFontSizeFailure1 = newPos "badFontSizeFailure1.fonts" 7 11

badFontSizeFailure2 :: SourcePos
badFontSizeFailure2 = newPos "badFontSizeFailure2.fonts" 4 8

badFontStylesFailure1 :: SourcePos
badFontStylesFailure1 = newPos "badFontStylesFailure1.fonts" 4 15 

badFontStylesFailure2 :: SourcePos
badFontStylesFailure2 = newPos "badFontStylesFailure2.fonts" 2 16

newLineEndFailure :: SourcePos
newLineEndFailure = newPos "newLineEndFailure.fonts" 3 12
