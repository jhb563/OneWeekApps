-- OWAAlertParse will expose the method:
-- parseAlertsFromFile :: FilePath -> IO [OWAAlert]
-- which will read a file a return a list of alert
-- objects for the alerts described in the files

module AlertParseTests (
  runAlertParseTests
) where

import OWAAlert
import OWAAlertParser
import TestAlerts
import Test.Hspec

runAlertParseTests :: FilePath -> IO ()
runAlertParseTests startFilePath = hspec $ do
  let parseFilesPath = startFilePath ++ "/tests/AlertTests/AlertParseFiles"
  oneButtonAlertTests parseFilesPath
  multiButtonAlertTests parseFilesPath
  missingTitleOrMessageAlertTests parseFilesPath
  characterKeyAlerts parseFilesPath

oneButtonAlertTests :: FilePath -> Spec
oneButtonAlertTests testDirectory = do
  let oneButtonAlertTestsFile = testDirectory ++ oneButtonAlertsExtension
  describe "Parse Alerts with dismiss or neutral button" $
    context "with any amount of spacing" $
    context "with attributes in any order" $
      it "Should match our list of one button alerts" $
        parseAlertsFromFile oneButtonAlertTestsFile `shouldReturn` oneButtonAlertsList

multiButtonAlertTests :: FilePath -> Spec
multiButtonAlertTests testDirectory = do
  let multiButtonAlertTestsFile = testDirectory ++ multiButtonAlertsExtension
  describe "Parse Alerts with yes/no button format" $
    it "Should match our list of multi button alerts" $
      parseAlertsFromFile multiButtonAlertTestsFile `shouldReturn` multiButtonAlertsList
 
missingTitleOrMessageAlertTests :: FilePath -> Spec
missingTitleOrMessageAlertTests testDirectory = do
  let missingTitleOrMessageAlertTestsFile = testDirectory ++ missingTitleOrMessageAlertsExtension
  describe "Parse Alerts with missing title or message" $
    it "Should match our list of missing title/message alerts" $
      parseAlertsFromFile missingTitleOrMessageAlertTestsFile `shouldReturn` missingTitleOrMessageAlertsList

characterKeyAlerts :: FilePath -> Spec
characterKeyAlerts testDirectory = do
  let characterKeyAlertTestsFile = testDirectory ++ characterKeyAlertsExtension 
  describe "Parse Alerts with quotes, escape characters, unusual characters" $
    it "Should match our list of unusual character alerts" $
      parseAlertsFromFile characterKeyAlertTestsFile `shouldReturn` characterKeyAlertsList

oneButtonAlertsExtension :: String
oneButtonAlertsExtension = "/oneButtonAlerts.alerts"

multiButtonAlertsExtension :: String
multiButtonAlertsExtension = "/multipleButtonAlerts.alerts"

missingTitleOrMessageAlertsExtension :: String
missingTitleOrMessageAlertsExtension = "/missingTitleOrMessageAlerts.alerts"

characterKeyAlertsExtension :: String
characterKeyAlertsExtension = "/characterKeyAlerts.alerts"

oneButtonAlertsList :: [OWAAlert]
oneButtonAlertsList = [myFirstAlert, secondAlert, keyAlert]

multiButtonAlertsList :: [OWAAlert]
multiButtonAlertsList = [twoButtonAlert, twoButtonsNoKeys]

missingTitleOrMessageAlertsList :: [OWAAlert]
missingTitleOrMessageAlertsList = [noTitleAlert, noMessageAlert, blankMessage]

characterKeyAlertsList :: [OWAAlert]
characterKeyAlertsList = [escapedQuotes, otherEscapeCharacters]
