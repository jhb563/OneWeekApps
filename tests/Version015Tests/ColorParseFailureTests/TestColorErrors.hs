module TestColorErrors where

import OWAParseError

colorKeyword1FailureInfo :: ErrorInfo
colorKeyword1FailureInfo = ("colorKeyword1Failure.colors", 1, 1)

colorKeyword2FailureInfo :: ErrorInfo
colorKeyword2FailureInfo = ("colorKeyword2Failure.colors", 5, 1)

colorName1FailureInfo :: ErrorInfo
colorName1FailureInfo = ("colorName1Failure.colors", 1, 7)

colorName2FailureInfo :: ErrorInfo
colorName2FailureInfo = ("colorName2Failure.colors", 6, 7)

colorName3FailureInfo :: ErrorInfo
colorName3FailureInfo = ("colorName3Failure.colors", 1, 7)

badAttribute1FailureInfo :: ErrorInfo
badAttribute1FailureInfo = ("badAttribute1Failure.colors", 7, 3)

badAttribute2FailureInfo :: ErrorInfo
badAttribute2FailureInfo = ("badAttribute2Failure.colors", 3, 3)

float1FailureInfo :: ErrorInfo
float1FailureInfo = ("float1Failure.colors", 3, 8)

float2FailureInfo :: ErrorInfo
float2FailureInfo = ("float2Failure.colors", 3, 11)

hex1FailureInfo :: ErrorInfo
hex1FailureInfo = ("hex1Failure.colors", 2, 7)

hex2FailureInfo :: ErrorInfo
hex2FailureInfo = ("hex2Failure.colors", 2, 12)

hex3FailureInfo :: ErrorInfo
hex3FailureInfo = ("hex3Failure.colors", 2, 16)

hex4FailureInfo :: ErrorInfo
hex4FailureInfo = ("hex4Failure.colors", 5, 17)

newLineFailureInfo :: ErrorInfo
newLineFailureInfo = ("newLineEndFailure.colors", 2, 15)
