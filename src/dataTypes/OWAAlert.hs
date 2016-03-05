{-|
Module      : OWAAlert
Description : Module for alert model. All Strings except name are localized keys.
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAAlert where

type LocalizedKey = String

-- | Choice for format of buttons. Can either be a simple dismiss button (which
-- closes the alert with no function), a neutral button (which might take a handler)
-- or a yes/no set of buttons, which each have their own handler.
data AlertButtonFormat = DismissButton LocalizedKey |
  NeutralButton LocalizedKey |
  YesNoButtons LocalizedKey LocalizedKey
  deriving (Show, Eq)

-- | Alert model, which requires a name, title, message, and a button format
data OWAAlert = OWAAlert {
  alertName :: String,
  alertTitle :: LocalizedKey,
  alertMessage :: LocalizedKey,
  alertButtonFormat :: AlertButtonFormat
} deriving (Show, Eq)

