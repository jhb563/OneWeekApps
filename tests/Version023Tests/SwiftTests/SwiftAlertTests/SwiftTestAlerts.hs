module SwiftTestAlerts where

import OWAAlert

swiftTestAlerts :: [OWAAlert]
swiftTestAlerts = [myFirstAlert,
  secondAlert,
  keyAlert,
  twoButtonAlert,
  twoButtonsNoKeys,
  noTitleAlert,
  noMessageAlert,
  blankMessage,
  escapedQuotes,
  otherEscapeCharacters]
 
myFirstAlert :: OWAAlert
myFirstAlert = OWAAlert {
  alertName = "myFirstAlert",
  alertTitle = "Error",
  alertMessage = "You have encountered a fatal error. Goodbye",
  alertButtonFormat = DismissButton "OK"
}

secondAlert :: OWAAlert
secondAlert = OWAAlert {
  alertName = "secondAlert",
  alertTitle = "Game Over",
  alertMessage = "Sorry, you appear to have lost.",
  alertButtonFormat = NeutralButton "That Sucks"
}

keyAlert :: OWAAlert
keyAlert = OWAAlert {
  alertName = "keyAlert",
  alertTitle = "KEY_ALERT_TITLE",
  alertMessage = "KEY_ALERT_MESSAGE",
  alertButtonFormat = NeutralButton "KEY_ALERT_BUTTON_TITLE"
} 

twoButtonAlert :: OWAAlert
twoButtonAlert = OWAAlert {
  alertName = "twoButtonAlert",
  alertTitle = "QUIT_APP_TITLE",
  alertMessage = "SHOULD_QUIT_APP",
  alertButtonFormat = YesNoButtons "YES" "NO" 
} 

twoButtonsNoKeys :: OWAAlert
twoButtonsNoKeys = OWAAlert {
  alertName = "twoButtonsNoKeys",
  alertTitle = "Quit App?",
  alertMessage = "Do you want to quit the app?",
  alertButtonFormat = YesNoButtons "Yes" "No"
} 

noTitleAlert :: OWAAlert
noTitleAlert = OWAAlert {
  alertName = "noTitleAlert",
  alertTitle = "",
  alertMessage = "NO_TITLE_MESSAGE",
  alertButtonFormat = DismissButton "Wut"
} 

noMessageAlert :: OWAAlert
noMessageAlert = OWAAlert {
  alertName = "noMessageAlert",
  alertTitle = "YOU_WIN",
  alertMessage = "",
  alertButtonFormat = DismissButton "GREAT"
} 

blankMessage :: OWAAlert
blankMessage = OWAAlert {
  alertName = "blankMessage",
  alertTitle = "",
  alertMessage = "",
  alertButtonFormat = NeutralButton "NOTHING"
} 

escapedQuotes :: OWAAlert
escapedQuotes = OWAAlert {
  alertName = "escapedQuotes",
  alertTitle = "KEY_TITLE",
  alertMessage = "This message has \\\"quotes\\\" in it",
  alertButtonFormat = NeutralButton "\\\"Hi\\\""
} 

otherEscapeCharacters :: OWAAlert
otherEscapeCharacters = OWAAlert {
  alertName = "otherEscapeCharacters",
  alertTitle = "2TITLE",
  alertMessage = "!./*&()[]15hasdf123\\n\\r\\t",
  alertButtonFormat = YesNoButtons "..." "Whyyyy"
} 
