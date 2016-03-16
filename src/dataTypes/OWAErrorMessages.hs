{-|
Module      : OWAErrorMessages
Description : Module listing all error messages used in OWA
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAErrorMessages where

-------------------------------------------------------------------------------
-------------------GENERIC ERROR MESSAGES--------------------------------------
-------------------------------------------------------------------------------

-- | Message given when an item name is not valid.
validNameErrorMsg :: String
validNameErrorMsg = "Expected valid name. Names must begin with lowercase letters and be alphanumeric"

-- | Message given when a float value is not valid.
validFloatAttributeErrorMsg :: String
validFloatAttributeErrorMsg = "Expected valid float value."

-- | Message given when a file does not end in a new line character.
newLineErrorMsg :: String
newLineErrorMsg = "Your file must end in a new line!"

-------------------------------------------------------------------------------
-------------------COLOR ERROR MESSAGES----------------------------------------
-------------------------------------------------------------------------------

-- | Message given when the Color keyword is expected.
colorKeywordErrorMessage :: String
colorKeywordErrorMessage = "Expected Color"

-- | Message given when a color attribute name is expected.
validColorAttributeErrorMsg :: String
validColorAttributeErrorMsg = "Expected valid color attribute, such as Red, Green, Blue, Alpha, or Hex"

-- | Message given when a hex value is not valid.
validHexAttributeErrorMsg :: String
validHexAttributeErrorMsg = "Expected valid hex valid (either 6 or 8 hex digits)"

