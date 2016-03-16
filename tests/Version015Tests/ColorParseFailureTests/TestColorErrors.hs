module TestColorErrors where

import OWAErrorMessages
import OWAParseError

colorKeyword1FailureInfo :: ErrorInfo
colorKeyword1FailureInfo = ("/colorKeyword1Failure.colors", 1, 2, colorKeywordErrorMessage)

colorKeyword2FailureInfo :: ErrorInfo
colorKeyword2FailureInfo = ("/colorKeyword2Failure.colors", 5, 1, colorKeywordErrorMessage)

colorName1FailureInfo :: ErrorInfo
colorName1FailureInfo = ("/colorName1Failure.colors", 1, 7, validNameErrorMsg)

colorName2FailureInfo :: ErrorInfo
colorName2FailureInfo = ("/colorName2Failure.colors", 6, 7, validNameErrorMsg)

colorName3FailureInfo :: ErrorInfo
colorName3FailureInfo = ("/colorName3Failure.colors", 1, 7, validNameErrorMsg)

badAttribute1FailureInfo :: ErrorInfo
badAttribute1FailureInfo = ("/badAttribute1Failure.colors", 7, 3, validColorAttributeErrorMsg)

badAttribute2FailureInfo :: ErrorInfo
badAttribute2FailureInfo = ("/badAttribute2Failure.colors", 3, 3, validColorAttributeErrorMsg)

float1FailureInfo :: ErrorInfo
float1FailureInfo = ("/float1Failure.colors", 2, 10, validFloatAttributeErrorMsg)

float2FailureInfo :: ErrorInfo
float2FailureInfo = ("/float2Failure.colors", 3, 8, validFloatAttributeErrorMsg)

float3FailureInfo :: ErrorInfo
float3FailureInfo = ("/float3Failure.colors", 3, 11, validFloatAttributeErrorMsg)

hex1FailureInfo :: ErrorInfo
hex1FailureInfo = ("/hex1Failure.colors", 2, 7, validHexAttributeErrorMsg)

hex2FailureInfo :: ErrorInfo
hex2FailureInfo = ("/hex2Failure.colors", 2, 9, validHexAttributeErrorMsg)

hex3FailureInfo :: ErrorInfo
hex3FailureInfo = ("/hex3Failure.colors", 2, 9, validHexAttributeErrorMsg)

newLineFailureInfo :: ErrorInfo
newLineFailureInfo = ("/newLineFailre.colors", 2, 15, newLineErrorMsg)
