{-|
Module      : OWAAppInfoParser
Description : Module for parsing app info from a app.info file
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWAAppInfoParser (
  parseAppInfoFromFile
) where

import           Control.Monad.Identity
import           Data.List
import           Data.List.Split
import qualified Data.Map.Strict as Map
import           Text.Parsec
import           Text.ParserCombinators.Parsec

import           Model.OWAAppInfo
import           Model.OWAParseError
import           OWAParseUtil

type AppInfoAttr = String
type AppInfoVal = String
type AppInfoAttrMap = Map.Map AppInfoAttr AppInfoVal

-------------------------------------------------------------------------------
----------------------ENTRY METHODS--------------------------------------------
-------------------------------------------------------------------------------

-- | Takes a file, parses its contents, and creates a an appInfo object out
-- of the contents. (Or returns an error) 
parseAppInfoFromFile :: FilePath -> IO (Either [OWAParseError] OWAAppInfo)
parseAppInfoFromFile fPath = do
  contents <- readFile fPath
  let sourceName = sourceNameFromFile fPath
  let errorOrAppInfo = parseAppInfo sourceName contents
  case errorOrAppInfo of
    Left parseError -> return (Left [ParsecError parseError])
    Right (Left objectError) -> return $ Left [attachFileName sourceName objectError]
    Right (Right appInfo) -> return $ Right appInfo

parseAppInfo :: String -> String -> Either ParseError (Either OWAParseError OWAAppInfo)
parseAppInfo = Text.Parsec.runParser
  (do
    commentOrSpacesParser
    appInfo <- appInfoParser
    commentOrSpacesParser
    eof
    return appInfo)
  Identity
    
-------------------------------------------------------------------------------
----------------------PARSERS--------------------------------------------------
-------------------------------------------------------------------------------

appInfoParser :: GenParser Char st (Either OWAParseError OWAAppInfo)
appInfoParser = do
  attrs <- appInfoAttrLine `sepEndBy1` many (Text.Parsec.try singleTrailingComment)
  let attrMap = Map.fromList attrs
  let maybeAppInfo = appInfoFromAttrMap attrMap
  case maybeAppInfo of
    Nothing -> return $ Left ObjectError {
      fileName = "",
      itemName = "appInfo",
      missingRequiredAttributes = missingAttrs attrMap
    }
    Just appInfo -> return $ Right appInfo

appInfoAttrLine :: GenParser Char st (AppInfoAttr, AppInfoVal)
appInfoAttrLine = choice attrParsers

attrParsers :: [GenParser Char st (AppInfoAttr, AppInfoVal)]
attrParsers = map Text.Parsec.try
  [appNameParser,
  appPrefixParser,
  authorNameParser,
  dateCreatedParser,
  companyNameParser]

appNameParser :: GenParser Char st (AppInfoAttr, AppInfoVal)
appNameParser = colonParserWithKeyword appNameKeyword arbitraryStringParser

appPrefixParser :: GenParser Char st (AppInfoAttr, AppInfoVal)
appPrefixParser = colonParserWithKeyword appPrefixKeyword prefixParser

authorNameParser :: GenParser Char st (AppInfoAttr, AppInfoVal)
authorNameParser = colonParserWithKeyword authorNameKeyword arbitraryStringParser

dateCreatedParser :: GenParser Char st (AppInfoAttr, AppInfoVal)
dateCreatedParser = colonParserWithKeyword dateCreatedKeyword dateParser

companyNameParser :: GenParser Char st (AppInfoAttr, AppInfoVal)
companyNameParser = colonParserWithKeyword companyNameKeyword arbitraryStringParser

colonParserWithKeyword :: String -> GenParser Char st AppInfoVal -> GenParser Char st (AppInfoAttr, AppInfoVal)
colonParserWithKeyword keyword valParser = do
  string keyword
  char ':'
  spaceTabs
  appVal <- valParser
  return (keyword, appVal)

arbitraryStringParser :: GenParser Char st AppInfoVal
arbitraryStringParser = do
  totalString <- many1 (noneOf "\n")
  let afterCommentRemoval = head (splitOn "//" totalString)
  return $ reverse (dropWhile (\c -> c == ' ' || c == '\t') (reverse afterCommentRemoval))

prefixParser :: GenParser Char st AppInfoVal
prefixParser = count 3 upper

dateParser :: GenParser Char st AppInfoVal
dateParser = do
  monthString <- optionDigitCountParser 1 1
  char '/'
  dayString <- optionDigitCountParser 1 1
  char '/'
  yearString <- optionDigitCountParser 2 2
  return $ monthString ++ ('/':dayString) ++ ('/':yearString)

optionDigitCountParser :: Int -> Int -> GenParser Char st String
optionDigitCountParser numRequired numOptional = do
  requiredPortion <- count numRequired digit
  optionalPortion <- optionMaybe $ count numOptional digit
  case optionalPortion of
    Nothing -> return requiredPortion
    Just ds -> return $ requiredPortion ++ ds

-------------------------------------------------------------------------------
----------------------CONSTRUCTING APP INFO------------------------------------
-------------------------------------------------------------------------------

appInfoFromAttrMap :: AppInfoAttrMap -> Maybe OWAAppInfo
appInfoFromAttrMap attrMap = do
  appName <- Map.lookup appNameKeyword attrMap
  appPrefix <- Map.lookup appPrefixKeyword attrMap
  dateCreatedString <- Map.lookup dateCreatedKeyword attrMap
  authorName <- case Map.lookup authorNameKeyword attrMap of
    Nothing -> Just ""
    Just name -> Just name 
  return OWAAppInfo {
    appName = appName,
    appPrefix = appPrefix,
    authorName = authorName,
    dateCreatedString = dateCreatedString,
    companyName = Map.lookup companyNameKeyword attrMap
  }

missingAttrs :: AppInfoAttrMap -> [AppInfoAttr]
missingAttrs attrMap = requiredAttributes \\ Map.keys attrMap

-------------------------------------------------------------------------------
----------------------KEYWORD CONSTANTS----------------------------------------
-------------------------------------------------------------------------------

appNameKeyword :: String
appNameKeyword = "AppName"

appPrefixKeyword :: String
appPrefixKeyword = "Prefix"

authorNameKeyword :: String
authorNameKeyword = "Author"

dateCreatedKeyword :: String
dateCreatedKeyword = "Created"

companyNameKeyword :: String
companyNameKeyword = "Company"

requiredAttributes :: [String]
requiredAttributes = [appNameKeyword, appPrefixKeyword, dateCreatedKeyword]
