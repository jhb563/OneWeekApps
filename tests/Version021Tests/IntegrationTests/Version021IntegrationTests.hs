-- OWALib will expose a method:
-- runOWA :: FilePath -> [String] -> IO ()
-- which will run the One Week Apps program, searching for
-- an app directory, finding relevant files, and producing
-- the correct output files. We will test this on the cases
-- included in Version 0.2.1. These features include image
-- backgrounds for buttons, custom views, container views,
-- and scroll views.

module Version021IntegrationTests (
  runV021IntegrationTests
) where

import IntegrationTestUtil
import TestUtil
import Test.Hspec

runV021IntegrationTests :: FilePath :: IO ()
runV021IntegrationTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/tests/Version021Tests/IntegrationTests"
  runIntegrationTests testDirectory
  [checkViewsFiles]
  additionalFiles

checkViewsFiles :: FilePath -> Spec
checkViewsFiles testDirectory = do
  let view1Header = testDirectory ++ view1HeaderExtension
  let view1M = testDirectory ++ view1MExtension
  let view2Header = testDirectory ++ view2HeaderExtension
  let view2M = testDirectory ++ view2MExtension
  let view1HeaderTest = testDirectory ++ view1HeaderTestExtension
  let view1MTest = testDirectory ++ view1MTestExtension
  let view2HeaderTest = testDirectory ++ view2HeaderTestExtension
  let view2MTest = testDirectory ++ view2MTestExtension
  describe "Compare produced views files" $ do
    it "The first header should match" $
      view1Header `filesShouldMatch` view1HeaderTest    

    it "The first header should match" $
      view1M `filesShouldMatch` view1MTest    

    it "The first header should match" $
      view2Header `filesShouldMatch` view2HeaderTest    

    it "The first header should match" $
      view2M `filesShouldMatch` view2MTest    
  
additionalFiles :: [String]
additionalFiles = [view1HeaderExtension,
  view1MExtension,
  view2HeaderExtension,
  view2MExtension]

view1HeaderExtension :: String
view1HeaderExtension = "/app/MSAFirstView.h"

view1MExtension :: String
view1MExtension =  "/app/MSAFirstView.m"

view2HeaderExtension :: String
view2HeaderExtension = "/app/MSASecondView.h"

view2MExtension :: String
view2MExtension = "/app/MSASecondView.m"

view1HeaderTestExtension :: String
view1HeaderTestExtension = "/app/MSAFirstView.h.test"

view1MTestExtension :: String
view1MTestExtension =  "/app/MSAFirstView.m.test"

view2HeaderTestExtension :: String
view2HeaderTestExtension = "/app/MSASecondView.h.test"

view2MTestExtension :: String
view2MTestExtension = "/app/MSASecondView.m.test"
