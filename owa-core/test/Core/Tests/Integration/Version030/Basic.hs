-- OWALib will expose a method:
-- runOWA :: FilePath -> [String] -> IO ()
-- which will run the One Week Apps program, searching for
-- an app directory, finding relevant files, and producing
-- the correct output files. We will test this on the cases
-- included in Version 0.3.0, primarily model file generation.

module Core.Tests.Integration.Version030.Basic (
  runV030IntegrationTests
) where

import System.Directory (createDirectoryIfMissing)
import Test.Hspec

import Core.Tests.Integration.Utils
import Core.Tests.Utils

runV030IntegrationTests :: FilePath -> IO ()
runV030IntegrationTests currentDirectory = do
  let testDirectory = currentDirectory ++ "/test/Core/Tests/Integration/Version030"
  let outputDirectory = currentDirectory ++ "/test/Core/Tests/Integration/Version030/swift/IntegrationApp/"
  let outputDirectoryObjc = currentDirectory ++ "/test/Core/Tests/Integration/Version030/objc/IntegrationApp/"
  createDirectoryIfMissing True outputDirectory
  runIntegrationTestsObjc testDirectory [checkModelsFilesObjc] objcResults
  runIntegrationTestsSwift testDirectory [checkModelsFilesSwift] swiftResults

checkModelsFilesObjc :: FilePath -> Spec
checkModelsFilesObjc testDirectory = do
  let fullModelHeaderResult1 = testDirectory ++ modelHeaderResult1
  let fullModelHeaderResult2 = testDirectory ++ modelHeaderResult2
  let fullModelHeaderResult3 = testDirectory ++ modelHeaderResult3
  let fullModelHeaderTest1 = testDirectory ++ modelHeaderTest1
  let fullModelHeaderTest2 = testDirectory ++ modelHeaderTest2
  let fullModelHeaderTest3 = testDirectory ++ modelHeaderTest3
  let fullModelMResult1 = testDirectory ++ modelMResult1
  let fullModelMResult2 = testDirectory ++ modelMResult2
  let fullModelMResult3 = testDirectory ++ modelMResult3
  let fullModelMTest1 = testDirectory ++ modelMTest1
  let fullModelMTest2 = testDirectory ++ modelMTest2
  let fullModelMTest3 = testDirectory ++ modelMTest3
  describe "Compare Produced Objective C Models Files" $ do
    it "The first model header should match" $
      fullModelHeaderResult1 `filesShouldMatch` fullModelHeaderTest1

    it "The first model implementation should match" $
      fullModelMResult1 `filesShouldMatch` fullModelMTest1

    it "The second model header should match" $
      fullModelHeaderResult2 `filesShouldMatch` fullModelHeaderTest2

    it "The second model implementation should match" $
      fullModelMResult2 `filesShouldMatch` fullModelMTest2

    it "The third model header should match" $
      fullModelHeaderResult3 `filesShouldMatch` fullModelHeaderTest3

    it "The third model implementation should match" $
      fullModelMResult3 `filesShouldMatch` fullModelMTest3

checkModelsFilesSwift :: FilePath -> Spec
checkModelsFilesSwift testDirectory = do
  let fullModelResult1 = testDirectory ++ modelSwiftResult1
  let fullModelResult2 = testDirectory ++ modelSwiftResult2
  let fullModelResult3 = testDirectory ++ modelSwiftResult3
  let fullModelTest1 = testDirectory ++ modelSwiftTest1
  let fullModelTest2 = testDirectory ++ modelSwiftTest2
  let fullModelTest3 = testDirectory ++ modelSwiftTest3
  describe "Compare Produced Swift Models Files" $ do
    it "The first model should match" $
      fullModelResult1 `filesShouldMatch` fullModelTest1

    it "The second model should match" $
      fullModelResult2 `filesShouldMatch` fullModelTest2

    it "The third model should match" $
      fullModelResult3 `filesShouldMatch` fullModelTest3

objcResults :: [FilePath]
objcResults =
  [ modelHeaderResult1
  , modelHeaderResult2
  , modelHeaderResult3
  , modelMResult1
  , modelMResult2
  , modelMResult3
  ]

swiftResults :: [FilePath]
swiftResults =
  [ modelSwiftResult1
  , modelSwiftResult2
  , modelSwiftResult3
  ]

modelHeaderResult1 :: String
modelHeaderResult1 = "/objc/IntegrationApp/MyFirstModel.h"

modelHeaderResult2 :: String
modelHeaderResult2 = "/objc/IntegrationApp/CustomReferenceModel.h"

modelHeaderResult3 :: String
modelHeaderResult3 = "/objc/IntegrationApp/MyCompleteModel.h"

modelMResult1 :: String
modelMResult1 = "/objc/IntegrationApp/MyFirstModel.m"

modelMResult2 :: String
modelMResult2 = "/objc/IntegrationApp/CustomReferenceModel.m"

modelMResult3 :: String
modelMResult3 = "/objc/IntegrationApp/MyCompleteModel.m"

modelSwiftResult1 :: String
modelSwiftResult1 = "/swift/IntegrationApp/MyFirstModel.swift"

modelSwiftResult2 :: String
modelSwiftResult2 = "/swift/IntegrationApp/CustomReferenceModel.swift"

modelSwiftResult3 :: String
modelSwiftResult3 = "/swift/IntegrationApp/MyCompleteModel.swift"

modelHeaderTest1 :: String
modelHeaderTest1 = "/objc/IntegrationApp/MyFirstModel.h.test"

modelHeaderTest2 :: String
modelHeaderTest2 = "/objc/IntegrationApp/CustomReferenceModel.h.test"

modelHeaderTest3 :: String
modelHeaderTest3 = "/objc/IntegrationApp/MyCompleteModel.h.test"

modelMTest1 :: String
modelMTest1 = "/objc/IntegrationApp/MyFirstModel.m.test"

modelMTest2 :: String
modelMTest2 = "/objc/IntegrationApp/CustomReferenceModel.m.test"

modelMTest3 :: String
modelMTest3 = "/objc/IntegrationApp/MyCompleteModel.m.test"

modelSwiftTest1 :: String
modelSwiftTest1 = "/swift/IntegrationApp/MyFirstModel.swift.test"

modelSwiftTest2 :: String
modelSwiftTest2 = "/swift/IntegrationApp/CustomReferenceModel.swift.test"

modelSwiftTest3 :: String
modelSwiftTest3 = "/swift/IntegrationApp/MyCompleteModel.swift.test"
