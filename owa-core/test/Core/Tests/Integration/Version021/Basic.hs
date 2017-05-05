-- OWALib will expose a method:
-- runOWA :: FilePath -> [String] -> IO ()
-- which will run the One Week Apps program, searching for
-- an app directory, finding relevant files, and producing
-- the correct output files. We will test this on the cases
-- included in Version 0.2.1. These features include image
-- backgrounds for buttons, custom views, container views,
-- and scroll views.

module Core.Tests.Integration.Version021.Basic (
  runV021IntegrationTests
) where

import Test.Hspec

import Core.Tests.Integration.Utils
import Core.Tests.Utils

runV021IntegrationTests :: FilePath -> IO ()
runV021IntegrationTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/test/Core/Tests/Integration/Version021"
  runIntegrationTests testDirectory
    [checkViewsFilesObjc]
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
view1HeaderExtension = "/objc/IntegrationApp/MSAFirstView.h"

view1MExtension :: String
view1MExtension =  "/objc/IntegrationApp/MSAFirstView.m"

view2HeaderExtension :: String
view2HeaderExtension = "/objc/IntegrationApp/MSASecondView.h"

view2MExtension :: String
view2MExtension = "/objc/IntegrationApp/MSASecondView.m"

view1HeaderTestExtension :: String
view1HeaderTestExtension = "/objc/IntegrationApp/MSAFirstView.h.test"

view1MTestExtension :: String
view1MTestExtension =  "/objc/IntegrationApp/MSAFirstView.m.test"

view2HeaderTestExtension :: String
view2HeaderTestExtension = "/objc/IntegrationApp/MSASecondView.h.test"

view2MTestExtension :: String
view2MTestExtension = "/objc/IntegrationApp/MSASecondView.m.test"
