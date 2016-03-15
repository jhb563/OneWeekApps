module ParseErrorUtil (
  ErrorInfo,
  errorFromInfo,
  colorKeywordErrorMessage,
  validNameErrorMsg,
  validAttributeErrorMsg,
  validFloatAttributeErrorMsg,
  validHexAttributeErrorMsg,
  newLineErrorMsg
) where

import OWAParseError
import Text.Parsec.Error

-------------------------------------------------------------------------------
-------------------BUILDING PARSE ERRORS---------------------------------------
-------------------------------------------------------------------------------

type ErrorInfo = (SourceName, Line, Column, String)

errorFromInfo :: FilePath -> ErrorInfo -> OWAParseError
errorFromInfo filePath (name, line, col, msg) = newErrorMessage message srcPos
  where message = Expect msg
        srcPos = newPos (filePath ++ name) line col

-------------------------------------------------------------------------------
-------------------ERROR MESSAGES----------------------------------------------
-------------------------------------------------------------------------------

colorKeywordErrorMessage :: String
colorKeywordErrorMessage = "Expected Color"

validNameErrorMsg :: String
validNameErrorMsg = "Expected valid name. Names must begin with lowercase letters and be alphanumeric"

validAttributeErrorMsg :: String
validAttributeErrorMsg = "Expected valid color attribute, such as Red, Green, Blue, Alpha, or Hex"

validFloatAttributeErrorMsg :: String
validFloatAttributeErrorMsg = "Expected valid float value."

validHexAttributeErrorMsg :: String
validHexAttributeErrorMsg = "Expected valid hex valid (either 6 or 8 hex digits)"

newLineErrorMsg :: String
newLineErrorMsg = "Your file must end in a new line!"

