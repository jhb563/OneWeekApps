{-|
Module      : OWAAlert
Description : Module for alert model. All Strings except name are localized keys.
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAAlert where

type LocalizedKey = String

-- | Alert model, which requires a name, title, message, and a list
-- of buttons. There must be at least one button.
data OWAAlert = OWAAlert {
  alertName :: String,
  alertTitle :: LocalizedKey,
  alertMessage :: LocalizedKey,
  alertButtons :: [LocalizdKey]
}

