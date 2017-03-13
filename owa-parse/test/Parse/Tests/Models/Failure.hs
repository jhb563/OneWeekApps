-- This will test parseModelFromFile, out of the module
-- ModelParser, under conditions which should create
-- a parse failure.

module Parse.Tests.Models.Failure (
  runModelParseFailureTests
) where

import Test.Hspec

import Parse.ModelParser
import Parse.Tests.Models.Errors
import Parse.Tests.Utils (shouldMatchError, shouldReturnLefts)

runModelParseFailureTests :: FilePath -> IO ()
runModelParseFailureTests currentDirectory = hspec $ do
  let testDirectory = currentDirectory ++ testDirectoryExtension 
  modelErrorTests testDirectory

modelErrorTests :: FilePath -> Spec
modelErrorTests testDirectory = do
  let testFile1 = testDirectory ++ parseFailure1Extension
  let testFile2 = testDirectory ++ parseFailure2Extension
  let testFile3 = testDirectory ++ parseFailure3Extension
  let testFile4 = testDirectory ++ parseFailure4Extension
  let testFile5 = testDirectory ++ parseFailure5Extension
  let testFile6 = testDirectory ++ parseFailure6Extension
  let testFile7 = testDirectory ++ parseFailure7Extension
  let testFile8 = testDirectory ++ parseFailure8Extension
  let testFile9 = testDirectory ++ parseFailure9Extension
  let testFile10 = testDirectory ++ parseFailure10Extension
  let testFile11 = testDirectory ++ parseFailure11Extension
  let testFile12 = testDirectory ++ parseFailure12Extension
  let testFile13 = testDirectory ++ parseFailure13Extension
  let testFile14 = testDirectory ++ parseFailure14Extension
  let testFile15 = testDirectory ++ parseFailure15Extension
  let testFile16 = testDirectory ++ parseFailure16Extension
  describe "Parse an improper model file" $ do
    context "when the \"Model\" keyword is lowercased" $
      it "Should return a parse error highlighting the improper keyword" $
        parseModelFromFile testFile1 `shouldMatchError` lowercaseModelTagError

    context "when the \"Type\" keyword is lowercased" $
      it "Should return a parse error highlighting the improper keyword" $
        parseModelFromFile testFile2 `shouldMatchError` lowercaseTypeTagError

    context "when the \"Field\" keyword is lowercased" $
      it "Should return a parse error highlighting the improper keyword" $
        parseModelFromFile testFile3 `shouldMatchError` lowercaseFieldTagError

    context "when a \"Field\" name is uppercased" $
      it "Should return a parse error highlighting the improper name" $
        parseModelFromFile testFile4 `shouldMatchError` uppercaseFieldNameError

    context "when the \"Array\" keyword is lowercased" $
      it "Should return a parse error highlighting the improper tag" $
        parseModelFromFile testFile5 `shouldMatchError` lowercaseArrayTagError

    context "when the \"Readonly\" keyword is lowercased" $
      it "Should return a parse error highlighting the improper tag" $
        parseModelFromFile testFile6 `shouldMatchError` lowercaseReadonlyTagError

    context "when a custom class name is lowercased" $
      it "Should return a parse error highlighting the improper name" $
        parseModelFromFile testFile7 `shouldMatchError` lowercaseClassNameTagError

    context "when the invalid \"Readwrite\" tag is used" $
      it "Should return a parse error highlighting the improper tag" $
        parseModelFromFile testFile8 `shouldMatchError` readWriteError

    context "when the invalid \"Double\" tag is used" $
      it "Should return a parse error highlighting the improper tag" $
        parseModelFromFile testFile9 `shouldMatchError` doubleError

    context "when no secondary type is provided for a Maybe type" $
      it "Should return a parse error highlighting the lack of a type" $
        parseModelFromFile testFile10 `shouldMatchError` noMaybeTypeError

    context "when no secondary type is provided for a Array type" $
      it "Should return a parse error highlighting the lack of a type" $
        parseModelFromFile testFile11 `shouldMatchError` noArrayTypeError

    context "when no secondary type is provided for a Map type" $
      it "Should return a parse error highlighting the lack of a type" $
        parseModelFromFile testFile12 `shouldMatchError` noMapTypeError

    context "when no field name is provided" $
      it "Should return a parse error highlighting the lack of a name" $
        parseModelFromFile testFile13 `shouldMatchError` noFieldName

    context "when no type is provided for a field" $
      it "Should return a parse error highlighting the lack of a type" $
        parseModelFromFile testFile14 `shouldReturnLefts` noTypeForFieldError

    context "when an existing type name is used as a custom type" $
      it "Should return a parse error highlighting the lack of a type" $
        parseModelFromFile testFile15 `shouldMatchError` invalidCustomFieldError

    context "when a custom type does not use the custom keyword" $
      it "Should return a parse error highlighting the lack of a type" $
        parseModelFromFile testFile16 `shouldMatchError` noCustomTagError

testDirectoryExtension :: FilePath
testDirectoryExtension = "/test/Parse/Tests/Models/ParseFiles/"

parseFailure1Extension :: FilePath
parseFailure1Extension = "/parseFail1.model"

parseFailure2Extension :: FilePath
parseFailure2Extension = "/parseFail2.model"

parseFailure3Extension :: FilePath
parseFailure3Extension = "/parseFail3.model"

parseFailure4Extension :: FilePath
parseFailure4Extension = "/parseFail4.model"

parseFailure5Extension :: FilePath
parseFailure5Extension = "/parseFail5.model"

parseFailure6Extension :: FilePath
parseFailure6Extension = "/parseFail6.model"

parseFailure7Extension :: FilePath
parseFailure7Extension = "/parseFail7.model"

parseFailure8Extension :: FilePath
parseFailure8Extension = "/parseFail8.model"

parseFailure9Extension :: FilePath
parseFailure9Extension = "/parseFail9.model"

parseFailure10Extension :: FilePath
parseFailure10Extension = "/parseFail10.model"

parseFailure11Extension :: FilePath
parseFailure11Extension = "/parseFail11.model"

parseFailure12Extension :: FilePath
parseFailure12Extension = "/parseFail12.model"

parseFailure13Extension :: FilePath
parseFailure13Extension = "/parseFail13.model"

parseFailure14Extension :: FilePath
parseFailure14Extension = "/parseFail14.model"

parseFailure15Extension :: FilePath
parseFailure15Extension = "/parseFail15.model"

parseFailure16Extension :: FilePath
parseFailure16Extension = "/parseFail16.model"
