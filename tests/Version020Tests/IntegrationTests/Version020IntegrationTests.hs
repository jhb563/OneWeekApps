-- OWALib will expose a method:
-- runOWA :: FilePath -> [String] -> IO ()
-- which will run the One Week Apps program, searching for
-- an app directory, finding relevant files, and producing
-- the correct output files. We will test this on the cases
-- included in Version 0.1.5. These include localized string
-- creation, commenting of files, indentation flexibility,
-- parse failures, and the app.info files.

module Version020IntegrationTests (
  runV020IntegrationTests
) where

import IntegrationTestUtil
import TestUtil
import Test.Hspec

runV020IntegrationTests :: FilePath -> IO ()
runV020IntegrationTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/tests/Version020Tests/IntegrationTests"
  runIntegrationTests testDirectory 
    [checkColorsFiles,
    checkFontsFiles,
    checkViewsFiles] 
    additionalFiles

checkViewsFiles :: FilePath -> Spec
checkViewsFiles testDirectory = do
  let view1Header = testDirectory ++ view1HeaderExtension
  let view1M = testDirectory ++ view1MExtension
  let view2Header = testDirectory ++ view2HeaderExtension
  let view2M = testDirectory ++ view2MExtension
  let view3Header = testDirectory ++ view3HeaderExtension
  let view3M = testDirectory ++ view3MExtension
  let view1HeaderTest = testDirectory ++ view1HeaderTestExtension
  let view1MTest = testDirectory ++ view1MTestExtension
  let view2HeaderTest = testDirectory ++ view2HeaderTestExtension
  let view2MTest = testDirectory ++ view2MTestExtension
  let view3HeaderTest = testDirectory ++ view3HeaderTestExtension
  let view3MTest = testDirectory ++ view3MTestExtension
  describe "Compare Produced Views Files" $ do
    it "The first header should match" $
      view1Header `filesShouldMatch` view1HeaderTest
    
    it "The first implementation should match" $
      view1M `filesShouldMatch` view1MTest

    it "The second header should match" $
      view2Header `filesShouldMatch` view2HeaderTest
    
    it "The second implementation should match" $
      view2M `filesShouldMatch` view2MTest

    it "The third header should match" $
      view3Header `filesShouldMatch` view3HeaderTest
    
    it "The third implementation should match" $
      view3M `filesShouldMatch` view3MTest

additionalFiles :: [FilePath]
additionalFiles = [view1HeaderExtension,
  view1MExtension,
  view2HeaderExtension,
  view2MExtension,
  view3HeaderExtension,
  view3MExtension]

view1HeaderExtension :: String
view1HeaderExtension = "/app/VIAFirstView.h"

view1MExtension :: String
view1MExtension =  "/app/VIAFirstView.m"

view2HeaderExtension :: String
view2HeaderExtension = "/app/VIASecondView.h"

view2MExtension :: String
view2MExtension = "/app/VIASecondView.m"

view3HeaderExtension :: String
view3HeaderExtension = "/app/VIAThirdView.h"

view3MExtension :: String
view3MExtension = "/app/VIAThirdView.m"

view1HeaderTestExtension :: String
view1HeaderTestExtension = "/app/VIAFirstView.h.test"

view1MTestExtension :: String
view1MTestExtension =  "/app/VIAFirstView.m.test"

view2HeaderTestExtension :: String
view2HeaderTestExtension = "/app/VIASecondView.h.test"

view2MTestExtension :: String
view2MTestExtension = "/app/VIASecondView.m.test"

view3HeaderTestExtension :: String
view3HeaderTestExtension = "/app/VIAThirdView.h.test"

view3MTestExtension :: String
view3MTestExtension = "/app/VIAThirdView.m.test"
