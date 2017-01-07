module TestStringSets where

import qualified Data.Map.Strict as Map

import           OWALocalizedStringSet

basicStrings :: OWALocalizedStringSet
basicStrings = OWALocalizedStringSet {
  setName = "basicStringsTest",
  setMap = Map.fromList
    [("ALERT_TITLE", "First Alert"),
    ("ALERT_MESSAGE", "You have a new message"),
    ("FIRST_SCREEN_TITLE", "My First VC"),
    ("FIRST_SCREEN_LABEL_ITEM", "Hello!"),
    ("DifferentKeyFormat", "&and|or!not"),
    ("alllowercase", "What's up!")]
}

quotedStrings :: OWALocalizedStringSet
quotedStrings = OWALocalizedStringSet {
  setName = "quotedStringsTest",
  setMap = Map.fromList
    [("STRING_WITH_QUOTES", "You have \\\"great\\\" style"),
    ("SecondStringWithQuotes", "\\\"What is your name?\\\""),
    ("Empty String", ""),
    ("CONSECUTIVE_QUOTES", "\\\"\\\"Hahaha"),
    ("Key with \\\"quotes\\\"", "Yo")]
}

spacedStrings :: OWALocalizedStringSet
spacedStrings = OWALocalizedStringSet {
  setName = "spacedStringsTest",
  setMap = Map.fromList
    [("FIRST_VC_STRING1", "String 1"),
    ("FIRST_VC_STRING2", "String_2"),
    ("SECOND_VC_STRING1", "  Hello"),
    ("SECOND_VC_STRING2", "Yo  "),
    ("THIRD_VC_STRING1", "Title and Name"),
    ("THIRD_VC_STRING2", "What's app"),
    ("THIRD_VC_STRING3", "Didn't see that coming")]
}

commentedStrings :: OWALocalizedStringSet
commentedStrings = OWALocalizedStringSet {
  setName = "commentedStringsTest",
  setMap = Map.fromList
    [("STRING1", "Hi"),
    ("STRING2", "Bye"),
    ("STRING3", "Yo"),
    ("STRING4", "Yes")]
}
