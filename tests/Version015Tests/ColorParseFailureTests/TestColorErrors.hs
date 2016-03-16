module TestColorErrors where

import Text.Parsec.Pos

colorKeyword1FailureInfo :: SourcePos
colorKeyword1FailureInfo = newPos "colorKeyword1Failure.colors" 1 1

colorKeyword2FailureInfo :: SourcePos
colorKeyword2FailureInfo = newPos "colorKeyword2Failure.colors" 5 1

colorName1FailureInfo :: SourcePos
colorName1FailureInfo = newPos "colorName1Failure.colors" 1 7

colorName2FailureInfo :: SourcePos
colorName2FailureInfo = newPos "colorName2Failure.colors" 6 7

colorName3FailureInfo :: SourcePos
colorName3FailureInfo = newPos "colorName3Failure.colors" 1 7

badAttribute1FailureInfo :: SourcePos
badAttribute1FailureInfo = newPos "badAttribute1Failure.colors" 7 3

badAttribute2FailureInfo :: SourcePos
badAttribute2FailureInfo = newPos "badAttribute2Failure.colors" 3 3

float1FailureInfo :: SourcePos
float1FailureInfo = newPos "float1Failure.colors" 3 8

float2FailureInfo :: SourcePos
float2FailureInfo = newPos "float2Failure.colors" 3 11

hex1FailureInfo :: SourcePos
hex1FailureInfo = newPos "hex1Failure.colors" 2 7

hex2FailureInfo :: SourcePos
hex2FailureInfo = newPos "hex2Failure.colors" 2 12

hex3FailureInfo :: SourcePos
hex3FailureInfo = newPos "hex3Failure.colors" 2 16

hex4FailureInfo :: SourcePos
hex4FailureInfo = newPos "hex4Failure.colors" 5 17

newLineFailureInfo :: SourcePos
newLineFailureInfo = newPos "newLineEndFailure.colors" 2 15
