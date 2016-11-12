{-# LANGUAGE OverloadedStrings #-}

module OtherTemplates where

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

{-contentsTemplate :: Text
contentsTemplate = "
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<Workspace
   version = \"1.0\">
   <FileRef
      location = \"self:$projectname.xcodeproj\">
   </FileRef>
</Workspace>
"-}
