module FileSearchTests (
    runFileSearchTests
) where

import System.Directory

runFileSearchTests :: FilePath -> IO ()
runFileSearchTests currentDirectory = hspec $
  beforeAll_ (setupTestEnv currentDirectory)
  . afterAll_ (teardownTestEnv currentDirectory) $ do
    _ <- findBasicTests currentDirectory
    _ <- findColorsTests currentDirectory
    _ <- findFontsTests currentDirectory
    _ <- findAlertsTests currentDirectory
    _ <- findErrorsTests currentDirectory
    findFailureTests currentDirectory

findBasicTests :: FilePath -> Spec
findBasicTests currentDirectory = do
  let basicPath = currentDirectory ++ basicExtension
  describe "Find files in simple context" $ do
    it "Colors should be at /testenv/basic/myapp.colors" $
      findColorsFiles basicPath `shouldReturn` [basicPath ++ "myapp.colors"]

    it "Fonts should be at /testenv/basic/myapp.fonts" $
      findFontsFiles basicPath `shouldReturn` [basicPath ++ "myapp.fonts"]

    it "Alerts should be at /testenv/basic/myapp.alerts" $
      findAlertsFiles basicPath `shouldReturn` [basicPath ++ "myapp.alerts"]

    it "Errors should be at /testenv/basic/myapp.errors" $
      findErrorsFiles basicPath `shouldReturn` [basicPath ++ "myapp.errors"]

findColorsTests :: FilePath -> Spec
findColorsTests currentDirectory = do
  describe "Find Colors in hard context" $ do
    it "Complex" $
      pending

findFontsTests :: FilePath -> Spec
findFontsTests currentDirectory = do
  describe "Find Fonts in hard context" $ do
    it "Complex" $
      pending

findAlertsTests :: FilePath -> Spec
findAlertsTests currentDirectory = do
  describe "Find Alerts in hard context" $ do
    it "Complex" $
      pending

findErrorsTests :: FilePath -> Spec
findErrorsTests currentDirectory = do
  describe "Find Errors in hard context" $ do
    it "Complex" $
      pending

findFailureTests :: FilePath -> Spec
findFailureTests currentDirectory = do
  let failurePath = currentDirectory ++ failureExtension
  describe "Find files when none exist" $ do
    it "Should not find any colors"
      findColorsFiles failurePath `shouldReturn` []

    it "Should not find any fonts"
      findFontsFiles failurePath `shouldReturn` []

    it "Should not find any alerts"
      findAlertsFiles failurePath `shouldReturn` []

    it "Should not find any errors"
      findErrorsFiles failurePath `shouldReturn` []

testEnvExtension :: FilePath
testEnvExtension = "/testenv"

basicExtension :: FilePath
basicExtension = "/testenv/basic"

complexExtension :: FilePath
complexExtension = "/testenv/complex"

failureExtension :: FilePath
failureExtension = "/testenv/failure"

setupTestEnv :: FilePath -> IO ()
setupTestEnv currentDirectory = createDirectoryIfMissing True $ currentDirectory ++ testEnvExtension

teardownTestEnv :: FilePath -> IO ()
teardownTestEnv currentDirectory = removeDirectoryRecursive (currentDirectory ++ testEnvExtension)

-- OWAFileSearch will expose the following methods:
-- findColorsFiles :: FilePath -> IO [FilePath]
-- findFontsFiles :: FilePath -> IO [FilePath]
-- findAlertsFiles :: FilePath -> IO [FilePath]
-- findErrorsFiles :: FilePath -> IO [FilePath]
-- We will pass an "app" directory (though in these tests it might
-- not necessarily be called app), and these methods will search for all
-- files with the respective extensions:
-- .colors, .fonts, .alerts, .errors

-- Setup Directory Structure
-- | testenv
-- -- | basic
-- -- -- | myapp.colors
-- -- -- | myapp.fonts
-- -- -- | myapp.alerts
-- -- -- | myapp.errors
-- -- | complex
-- -- -- | level1a
-- -- -- -- | l1nothing
-- -- -- -- | level2a
-- -- -- -- -- | l2a.fonts
-- -- -- -- | l1a.alerts
-- -- -- -- | l1a.errors
-- -- -- -- | l1a2.errors
-- -- -- -- | distraction.js
-- -- -- | level1b
-- -- -- -- | level2b
-- -- -- -- -- | l2b.colors
-- -- -- -- -- | color.font
-- -- -- -- | l1b.colors
-- -- -- -- | l1b.errors
-- -- -- | level1c
-- -- -- -- | l1c.alerts
-- -- -- -- | l1c2.alerts
-- -- -- -- | l1c.fonts
-- -- -- | l0.colors
-- -- -- | l0.fonts
-- -- -- | l02.colors
-- -- -- | l0.alerts
-- -- -- | color.xml
-- -- -- | random.app
-- -- | failure
-- -- -- | failLevel1a
-- -- -- -- | failLevel2
-- -- -- -- -- | nothing.here
-- -- -- -- | xml.font
-- -- -- -- | spec.alert
-- -- -- | failLevel1b
-- -- -- | fonts.xml
-- -- -- | alerts.json
-- -- -- | errors.error

-- 4 tests for each type
-- 1. testenv/basic
-- 2. testenv/complex
-- 3. A subdirectory of complex
-- 4. testenv/failure (result is always [])
-- Expected Results (all results get sorted)
-- colors
-- 1. [myapp.colors]
-- 2. [l0.colors, l02.colors, l2b.colors]
-- 3. (use testenv/complex/level1b) [l2b.colors]
-- fonts
-- 1. [myapp.fonts]
-- 2. [l0.fonts, l2a.fonts]
-- 3. (use testenv/complex/level1a) [l2a.fonts]
-- alerts
-- 1. [myapp.alerts]
-- 2. [l0.alerts, l1a.alerts, l1c.alerts, l1c2.alerts]
-- 3. (use testenv/complex/level1c) [l1c.alerts, l1c2.alerts]
-- errors
-- 1. [myapp.errors]
-- 2. [l1a.errors, l1a2.errors, l1b.errors]
-- 3. (use testenv/complex/level1a) [l1a.errors, l1a2.errors]