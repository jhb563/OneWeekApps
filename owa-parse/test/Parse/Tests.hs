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
import Parse.Tests.Fonts.Failure (runFontParseFailureTests)
import Parse.Tests.Models.Basic (runModelParseTests)
import Parse.Tests.Spacing.Basic (runSpacingIndentTests)
import Parse.Tests.Strings.Basic (runStringsParseTests)
import Parse.Tests.Tabs.Basic (runTabTests)
import Parse.Tests.Views.Constraints (runViewConstraintTests)
import Parse.Tests.Views.Containers (runContainerViewParseTests)
import Parse.Tests.Views.Custom (runCustomViewParseTests)
import Parse.Tests.Views.Elements (runViewElementTests)
import Parse.Tests.Views.Failure (runViewFailureTests)
import Parse.Tests.Views.Names (runViewNameTests)

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
  runFontParseFailureTests currentDirectory
  runSpacingIndentTests currentDirectory
  runStringsParseTests currentDirectory
  runTabTests currentDirectory
  runViewNameTests currentDirectory
  runViewElementTests currentDirectory
  runViewConstraintTests currentDirectory 
  runViewFailureTests currentDirectory
  runContainerViewParseTests currentDirectory
  runCustomViewParseTests currentDirectory
  runModelParseTests
