-- OWAErrorParser will expose the method:
-- parseErrorsFromFile :: FilePath -> IO [OWAError]
-- which will read a file and return a list of font
-- objects for the fonts described in the file

module ErrorParseTests (
  runErrorParseTests
) where

import OWAError
import OWAErrorParser
import TestErrors
import Test.Hspec

runErrorParseTests :: FilePath -> IO ()
runErrorParseTests startFilePath = hspec $ do
  let parseFilesPath = startFilePath ++ "/tests/Version010Tests/ErrorTests/ErrorParseFiles" 
  noDefaultDomainTests parseFilesPath
  singleDomainTests parseFilesPath
  singleDomainPrefixTests parseFilesPath
  multiDomainTests parseFilesPath

noDefaultDomainTests :: FilePath -> Spec
noDefaultDomainTests testDirectory = do
  let noDefaultDomainTestsFile = testDirectory ++ noDefaultDomainExtension
  describe "Parse Errors with no default domains" $
    it "Should match our list of regular errors" $
      parseErrorsFromFile noDefaultDomainTestsFile `shouldReturn` regularErrorsList

singleDomainTests :: FilePath -> Spec
singleDomainTests testDirectory = do
  let singleDomainTestsFile = testDirectory ++ singleDomainExtension
  describe "Parse Errors with a single default domain" $
    it "Should match our list of single default domain errors" $
      parseErrorsFromFile singleDomainTestsFile `shouldReturn` singleDomainErrorsList

singleDomainPrefixTests :: FilePath -> Spec
singleDomainPrefixTests testDirectory = do
  let singleDomainPrefixTestsFile = testDirectory ++ singleDomainPrefixExtension
  describe "Parse Errors with a prefixed domain" $
    it "Should match our list of prefixed domain errors" $
      parseErrorsFromFile singleDomainPrefixTestsFile `shouldReturn` singleDomainPrefixErrorsList

multiDomainTests :: FilePath -> Spec
multiDomainTests testDirectory = do
  let multiDomainTestsFile = testDirectory ++ multiDomainExtension
  describe "Parse Errors with multiple domains in the file" $
    it "Should match our list of multi-domain errors" $
      parseErrorsFromFile multiDomainTestsFile `shouldReturn` multiDomainErrorsList

noDefaultDomainExtension :: String
noDefaultDomainExtension = "/noDefaultDomainErrors.errors"

singleDomainExtension :: String
singleDomainExtension = "/singleDomainErrors.errors"

singleDomainPrefixExtension :: String
singleDomainPrefixExtension  = "/singleDomainPrefixErrors.errors"

multiDomainExtension :: String
multiDomainExtension = "/multipleDomainErrors.errors"

regularErrorsList :: [OWAError]
regularErrorsList = [myError1, disconnectError, authError, sadError, userNotFound]

singleDomainErrorsList :: [OWAError]
singleDomainErrorsList = [simpleError, noPrefixError, redundantDomain, differentDomain]

singleDomainPrefixErrorsList :: [OWAError]
singleDomainPrefixErrorsList = [firstPrefixedError, unprefixedError]

multiDomainErrorsList :: [OWAError]
multiDomainErrorsList = [beforeDomainSpec, whatError, whichError, interloperDomain, finalError]