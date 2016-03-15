module TestColorErrors where

import ParseErrorUtil

colorKeyword1Failure :: ErrorInfo
colorKeyword1Failure = ("/colorKeyword1Failure.colors", 1, 2, colorKeywordErrorMessage)

colorKeyword2Failure :: ErrorInfo
colorKeyword2Failure = ("/colorKeyword2Failure.colors", 5, 1, colorKeywordErrorMessage)

colorName1Failure :: ErrorInfo
colorName1Failure = ("/colorName1Failure.colors", 1, 7, validNameErrorMsg)

colorName2Failure :: ErrorInfo
colorName2Failure = ("/colorName2Failure.colors", 6, 7, validNameErrorMsg)

colorName3Failure :: ErrorInfo
colorName3Failure = ("/colorName3Failure.colors", 1, 7, validNameErrorMsg)

badAttribute1Failure :: ErrorInfo
badAttribute1Failure = ("/badAttribute1Failure.colors", 7, 3, validAttributeErrorMsg)

badAttribute2Failure :: ErrorInfo
badAttribute2Failure = ("/badAttribute2Failure.colors", 3, 3, validAttributeErrorMsg)

float1Failure :: ErrorInfo
float1Failure = ("/float1Failure.colors", 2, 10, validFloatAttributeErrorMsg)

float2Failure :: ErrorInfo
float2Failure = ("/float2Failure.colors", 3, 8, validFloatAttributeErrorMsg)

float3Failure :: ErrorInfo
float3Failure = ("/float3Failure.colors", 3, 11, validFloatAttributeErrorMsg)

hex1Failure :: ErrorInfo
hex1Failure = ("/hex1Failure.colors", 2, 7, hexAttributeErrorMsg)

hex2Failure :: ErrorInfo
hex2Failure = ("/hex2Failure.colors", 2, 9, hexAttributeErrorMsg)

hex3Failure :: ErrorInfo
hex3Failure = ("/hex3Failure.colors", 2, 9, hexAttributeErrorMsg)

newLineFailure :: ErrorInfo
newLineFailure = ("/newLineFailre.colors", 2, 15, newLineErrorMsg)
