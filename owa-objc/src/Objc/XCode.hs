{-|
Module      : Objc.XCode
Description : Start file structures for an Objective C project (app delegate and initial
              view controller)
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Objc.XCode
  ( initialVCHeader
  , initialVCImplementation
  , appDelegateHeader
  , appDelegateImplementation )
  where

import Model.OWAAppInfo
import Objc.AbSyn

initialVCHeader :: OWAAppInfo -> ObjcFile
initialVCHeader _ = ObjcFile []

initialVCImplementation :: OWAAppInfo -> ObjcFile
initialVCImplementation _ = ObjcFile []

appDelegateHeader :: OWAAppInfo -> ObjcFile
appDelegateHeader _ = ObjcFile []

appDelegateImplementation :: OWAAppInfo -> ObjcFile
appDelegateImplementation _ = ObjcFile []
