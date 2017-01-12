{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Core.XCode.Templates.Other
Description : Contains template strings for the Info.plist file and contents. 
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Core.XCode.Templates.Other where

import Data.Text

-- | Template string for info.plist. Uses String type since no interpolation is needed.
infoPListTemplate :: String
infoPListTemplate = "\
\<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
\<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n\
\<plist version=\"1.0\">\n\
\<dict>\n\
\  <key>CFBundleDevelopmentRegion</key>\n\
\  <string>en</string>\n\
\  <key>CFBundleExecutable</key>\n\
\  <string>$(EXECUTABLE_NAME)</string>\n\
\  <key>CFBundleIdentifier</key>\n\
\  <string>$(PRODUCT_BUNDLE_IDENTIFIER)</string>\n\
\  <key>CFBundleInfoDictionaryVersion</key>\n\
\  <string>6.0</string>\n\
\  <key>CFBundleName</key>\n\
\  <string>$(PRODUCT_NAME)</string>\n\
\  <key>CFBundlePackageType</key>\n\
\  <string>APPL</string>\n\
\  <key>CFBundleShortVersionString</key>\n\
\  <string>1.0</string>\n\
\  <key>CFBundleSignature</key>\n\
\  <string>????</string>\n\
\  <key>CFBundleVersion</key>\n\
\  <string>1</string>\n\
\  <key>LSRequiresIPhoneOS</key>\n\
\  <true/>\n\
\  <key>UIRequiredDeviceCapabilities</key>\n\
\  <array>\n\
\    <string>armv7</string>\n\
\  </array>\n\
\  <key>UISupportedInterfaceOrientations</key>\n\
\  <array>\n\
\    <string>UIInterfaceOrientationPortrait</string>\n\
\    <string>UIInterfaceOrientationLandscapeLeft</string>\n\
\    <string>UIInterfaceOrientationLandscapeRight</string>\n\
\  </array>\n\
\</dict>\n\
\</plist>\n"

-- | Template for contents file. Subsitutes the project name. 
contentsTemplate :: Text
contentsTemplate = "\
\<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
\<Workspace\n\
\   version = \"1.0\">\n\
\   <FileRef\n\
\      location = \"self:$projectname.xcodeproj\">\n\
\   </FileRef>\n\
\</Workspace>\n"
