module TestAlerts where

import OWAAlert

allTestAlerts :: [OWAAlert]
allTestAlerts = [myFirstAlert,
  secondAlert,
  keyAlert,
  twoButtonAlert,
  twoButtonsNoKeys,
  threeButtons,
  noTitleAlert,
  noMessageAlert,
  blankMessage,
  escapedQuotes,
  otherEscapeCharacters]
 
sortedTestAlerts :: [OWAAlert]
sortedTestAlerts = [blankMessage,
  escapedQuotes,
  keyAlert,
  myFirstAlert,
  noMessageAlert,
  noTitleAlert, 
  otherEscapeCharacters,
  secondAlert,
  threeButtons,
  twoButtonAlert,
  twoButtonsNoKeys]

myFirstAlert :: OWAAlert
myFirstAlert = OWAAlert {
  alertName = "myFirstAlert",
  alertTitle = "Error",
  alertMessage = "You have encountered a fatal error. Goodbye",
  alertButtons = ["OK"]
}

secondAlert :: OWAAlert
secondAlert = OWAAlert {
  alertName = "secondAlert",
  alertTitle = "Game Over",
  alertMessage = "Sorry, you appear to have lost."
  alertButtons = ["That Sucks"]
}

keyAlert :: OWAAlert
keyAlert = OWAAlert {
  alertName = "keyAlert",
  alertTitle = "KEY_ALERT_TITLE",
  alertMessage = "KEY_ALERT_MESSAGE",
  alertButtons = ["KEY_ALERT_BUTTON_TITLE"]
} 

twoButtonAlert :: OWAAlert
twoButtonAlert = OWAAlert {
  alertName = "twoButtonAlert",
  alertTitle = "QUIT_APP_TITLE",
  alertMessage = "SHOULD_QUIT_APP",
  alertButtons = ["YES", "NO"]
} 

twoButtonsNoKeys :: OWAAlert
twoButtonsNoKeys = OWAAlert {
  alertName = "twoButtonsNoKeys",
  alertTitle = "Quit App?",
  alertMessage = "Do you want to quit the app?",
  alertButtons = ["Yes", "No"]
} 

threeButtons :: OWAAlert
threeButtons = OWAAlert {
  alertName = "threeButtons",
  alertTitle = "WHICH-DOOR",
  alertMessage = "DOOR_OPTIONS",
  alertButtons = ["LEFT", "CENTER", "RIGHT"]
} 

noTitleAlert :: OWAAlert
noTitleAlert = OWAAlert {
  alertName = "noTitleAlert",
  alertTitle = "",
  alertMessage = "NO_TITLE_MESSAGE",
  alertButtons = ["Wut"]
} 

noMessageAlert :: OWAAlert
noMessageAlert = OWAAlert {
  alertName = "noMessageAlert",
  alertTitle = "YOU_WIN",
  alertMessage = "",
  alertButtons = ["GREAT"]
} 

blankMessage :: OWAAlert
blankMessage = OWAAlert {
  alertName = "blankMessage",
  alertTitle = "",
  alertMessage = "",
  alertButtons = ["NOTHING"]
} 

escapedQuotes :: OWAAlert
escapedQuotes = OWAAlert {
  alertName = "escaptedQuotes",
  alertTitle = "KEY_TITLE",
  alertMessage = "This message has \\\"quotes\\\" in it",
  alertButtons = ["\\\"Hi\\\""]
} 

otherEscapeCharacters :: OWAAlert
otherEscapeCharacters = OWAAlert {
  alertName = "otherEscapeCharacters",
  alertTitle = "2TITLE",
  alertMessage = "!./*&()[]15hasdf123\\n\\r\\t",
  alertButtons = ["...", "Whyyyy"]
} 

