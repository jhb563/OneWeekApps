{-|
Module      : OWAColorObjc
Description : Module for Converting OWAColors to Objective C objects
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAColorObjc (
  objcHeaderFromColors,
  objcImplementationFromColors
) where

import OWAColor
import OWAObjcAbSyn

objcHeaderFromColors :: String -> [OWAColor] -> ObjcFile
objcHeaderFromColors catName colors = ObjcFile []

objcImplementationFromColors :: String -> [OWAColor] -> ObjcFile
objcImplementationFromColors catName colors = ObjcFile []
