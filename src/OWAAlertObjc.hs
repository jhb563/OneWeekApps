{-|
Module      : OWAAlertObjc
Description : Module for Converting OWAAlerts to Objective C objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAAlertObjc (
  objcHeaderFromAlerts,
  objcImplementationFromAlerts
) where

import OWAAlert
import OWAObjcAbSyn

objcHeaderFromAlerts :: String -> [OWAAlert] -> ObjcFile
objcHeaderFromAlerts categoryName alerts = ObjcFile []

objcImplementationFromAlerts :: String -> [OWAAlert] -> ObjcFile
objcImplementationFromAlerts categoryName alerts = ObjcFile []

