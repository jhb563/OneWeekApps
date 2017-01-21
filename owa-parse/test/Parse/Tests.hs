module Main where

import System.Directory (getCurrentDirectory)

import Parse.Tests.Alerts.Basic (runAlertParseTests)
import Parse.Tests.Alerts.Failure (runAlertParseFailureTests)
import Parse.Tests.AppInfo.Basic (runAppInfoParseTests)
import Parse.Tests.Colors.Basic (runColorParseTests)
import Parse.Tests.Colors.Failure (runColorParseFailureTests)
import Parse.Tests.Comments.Basic (runCommentTests)
import Parse.Tests.Errors.Basic (runErrorParseTests)
import Parse.Tests.Errors.Failure (runErrorParseFailureTests)
import Parse.Tests.Fonts.Basic (runFontParseTests)
import Parse.Tests.Spacing.Basic (runSpacingIndentTests)
import Parse.Tests.Strings.Basic (runStringsParseTests)
import Parse.Tests.Tabs.Basic (runTabTests)

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory 
  runAlertParseTests currentDirectory
  runAlertParseFailureTests currentDirectory
  runAppInfoParseTests currentDirectory
  runColorParseTests currentDirectory
  runColorParseFailureTests currentDirectory
  runCommentTests currentDirectory
  runErrorParseTests currentDirectory
  runErrorParseFailureTests currentDirectory
  runFontParseTests currentDirectory
  runSpacingIndentTests currentDirectory
  runStringsParseTests currentDirectory
  runTabTests currentDirectory
