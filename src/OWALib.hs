{-|
Module      : OWALib
Description : Main Library Entry point for OneWeekApps
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module OWALib (
  runOWA
) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except (runExceptT, ExceptT, ExceptT(..))
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import Data.Char (toUpper, isAlpha)
import Data.Either
import Data.List (find)
import Data.List.Split (splitOn)
import Control.Exception (handle)
import Data.Maybe (isJust, fromJust, catMaybes)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock
import OWAAlert
import OWAAlertObjc
import OWAAlertSwift
import OWAAlertParser
import OWAAppInfo
import OWAAppInfoParser
import OWAColor
import OWAColorObjc
import OWAColorParser
import OWAColorSwift
import OWAError
import OWAErrorObjc
import OWAErrorSwift
import OWAErrorParser
import OWAFileSearch
import OWAFont
import OWAFontObjc
import OWAFontSwift
import OWAFontParser
import OWALocalizedStringSet
import OWAObjcPrint
import OWASwiftPrint
import OWAParseError
import OWAStringsParser
import OWAStringsObjc
import OWAView
import OWAViewObjc
import OWAViewSwift
import OWAViewParser
import System.Directory
import System.IO 

type OWAReaderT = ReaderT OWAReaderInfo IO

data OWAReaderInfo = OWAReaderInfo {
  outputMode       :: OutputMode,
  lastObjcGenTime  :: Maybe UTCTime,
  lastSwiftGenTime :: Maybe UTCTime,
  codeTypes        :: [OWACodeType],
  languageType     :: OWALanguageType,
  inputHandle      :: Handle,
  outputHandle     :: Handle
}

data OWACodeType = 
  CodeTypeColors |
  CodeTypeFonts |
  CodeTypeAlerts |
  CodeTypeErrors |
  CodeTypeViews |
  CodeTypeStrings
  deriving (Show, Eq)

data OWALanguageType =
  LanguageTypeObjc |
  LanguageTypeSwift
  deriving (Show, Eq)

-- | 'runOWA' is the main running method for the OWA program. It takes a filepath
-- for a directory to search from, and generates all files.
runOWA :: Handle -> Handle -> FilePath -> [String] -> IO ()
runOWA iHandle oHandle filePath args = if null args
  then hPutStrLn oHandle "owa: No command entered!"
  else case head args of
    "new" -> runNewCommand filePath initialReaderInfo
    "gen" -> genFiles
    "generate" -> genFiles
    unrecognizedCmd -> hPutStrLn oHandle $ "owa: unrecognized command \"" ++ unrecognizedCmd ++ "\"!"
    where
      types = codeTypesFromArgs args 
      lang = languageTypeFromArgs args
      initialReaderInfo = OWAReaderInfo 
        (outputModeFromArgs $ tail args) 
        Nothing 
        Nothing 
        types 
        lang 
        iHandle 
        oHandle
      genFiles = do
                   (lastObjcTime, lastSwiftTime) <- lastCodeGenerationTime filePath
                   let newReaderInfo = initialReaderInfo 
                                        { lastObjcGenTime = lastObjcTime
                                        , lastSwiftGenTime = lastSwiftTime }
                   runReaderT (runOWAReader filePath) newReaderInfo

runOWAReader :: FilePath -> OWAReaderT ()
runOWAReader filePath = do
  findAppDirectoryAndRun filePath generateFiles

generateFiles :: FilePath -> OWAReaderT ()
generateFiles appDirectory = do
  appInfo <- loadAppInfo appDirectory
  case appInfo of
    Nothing -> printIfNotSilent "Couldn't parse app info. Exiting."
    Just appInfo -> do
      produceColorsFiles appDirectory appInfo
      produceFontsFiles appDirectory appInfo
      produceAlertsFiles appDirectory appInfo
      produceErrorsFiles appDirectory appInfo
      produceStringsFile appDirectory appInfo
      produceViewsFiles appDirectory appInfo
      modifyLastGenTime appDirectory

findAppDirectoryAndRun :: FilePath -> (FilePath -> OWAReaderT ()) -> OWAReaderT ()
findAppDirectoryAndRun filePath action = do
  printIfNotSilent "Searching for app directory"
  maybeAppDirectory <- liftIO $ findAppDirectory filePath
  case maybeAppDirectory of 
    Nothing -> printIfNotSilent "Couldn't find app directory! Exiting"
    Just appDirectory -> do
      printIfNotSilent "Found app directory"
      action appDirectory

---------------------------------------------------------------------------
------------------------MAKING NEW PROJECT---------------------------------
---------------------------------------------------------------------------

-- Have methods for each input element which loop back to themselves
-- On invalid input but through an error when no more input (Either)
-- Create the App directory (if it doesn't exist)
-- When all elements have been read, print out the app file
runNewCommand :: FilePath -> OWAReaderInfo -> IO ()
runNewCommand filePath readerInfo = do
    runReaderT (printIfNotSilent "Creating new OWA project!") readerInfo
    maybeAppInfo <- runReaderT appInfoReaderAction readerInfo
    case maybeAppInfo of
      Nothing -> return ()
      Just appInfo -> do
        createAppInfo appInfo
        runReaderT 
          (printIfNotSilent ("Your new app \"" ++ appName appInfo ++ "\" has been created!")) 
          readerInfo
  where
    appInfoReaderAction = runMaybeT $ do
      appName_ <- readAppName
      appPrefix_ <- readAppPrefix
      author_ <- readAuthor
      companyName_ <- readCompany
      dateCreated_ <- dateCreatedStringFromTime <$> liftIO getCurrentTime
      return OWAAppInfo {
        appName = appName_,
        appPrefix = appPrefix_,
        authorName = author_,
        dateCreatedString = dateCreated_,
        companyName = companyName_
      }
    createAppInfo appInfo = do
      appDirectory <- createAppDirectory filePath
      writeAppInfoToDirectory appDirectory appInfo

createAppDirectory :: FilePath -> IO FilePath
createAppDirectory filePath = do
  createDirectoryIfMissing False appFilePath
  return appFilePath
  where
    appFilePath = filePath ++ "/app"

writeAppInfoToDirectory :: FilePath -> OWAAppInfo -> IO ()
writeAppInfoToDirectory appDirectory appInfo = do
  handle <- openFile appInfoFile WriteMode
  hPutStrLn handle ("AppName: " ++ appName appInfo)
  hPutStrLn handle ("Prefix: " ++ appPrefix appInfo)
  hPutStrLn handle ("Author: " ++ authorName appInfo)
  hPutStrLn handle ("Created: " ++ dateCreatedString appInfo)
  when (isJust (companyName appInfo)) $ 
    hPutStrLn handle ("Company: " ++ fromJust (companyName appInfo))
  hClose handle
  where
    appInfoFile = appDirectory ++ appInfoFileExtension

type OWAMaybeT a = MaybeT OWAReaderT a

readAppName :: OWAMaybeT String
readAppName = do
  lift $ printIfNotSilent "What is the name of your app: "
  action
  where
    action = do
      name <- owaReadLine
      if null name
        then do
          lift $ printIfNotSilent "Your app must have a (non-empty) name! Please enter one: "
          action
        else return name

readAppPrefix :: OWAMaybeT String
readAppPrefix = do
  lift $ printIfNotSilent "What is the prefix of your app (3 letters): "
  action
  where
    validatePrefix :: String -> Bool
    validatePrefix prefix = length prefix == 3 && all isAlpha prefix
    action = do
      prefix <- owaReadLine
      if not (validatePrefix prefix)
        then do
          lift $ printIfNotSilent "The prefix must be 3 letters! Please enter again: " 
          action
        else return $ map toUpper prefix

readAuthor :: OWAMaybeT String
readAuthor = do
  lift $ printIfNotSilent "What is the author's name: "
  owaReadLine

readCompany :: OWAMaybeT (Maybe String)
readCompany = do
  lift $ printIfNotSilent "What is your company name (optional): "
  company <- owaReadLine
  if null company
    then return Nothing
    else return (Just company)

dateCreatedStringFromTime :: UTCTime -> String
dateCreatedStringFromTime time = finalString
  where
    day = utctDay time
    (year, month, date) = toGregorian day
    finalString = show month ++ "/" ++ show date ++ "/" ++ show year

---------------------------------------------------------------------------
------------------------LAZY CODE GENERATION HANDLERS----------------------
---------------------------------------------------------------------------

lastCodeGenerationTime :: FilePath -> IO (Maybe UTCTime, Maybe UTCTime)
lastCodeGenerationTime filePath = do
  appDirectory <- findAppDirectory filePath
  case appDirectory of
    Nothing -> return (Nothing, Nothing)
    Just appDirectory -> do
      let lastGenFilePath = appDirectory ++ lastGenFileExtension
      lastGenExists <- doesFileExist lastGenFilePath
      if lastGenExists
        then do
          lastGenLines <- lines <$> readFile lastGenFilePath
          -- We use seq here to force evaluation of the whole list, as otherwise, for some strange reason
          -- it seems to keep the file open and cause permission issues later. 
          let parsedLines = seq (length lastGenLines) (catMaybes $ map parseLastGenLine lastGenLines)
          let lastObjc = snd <$> find (\(typ, _) -> typ == LanguageTypeObjc) parsedLines
          let lastSwift = snd <$> find (\(typ, _) -> typ == LanguageTypeSwift) parsedLines
          return (lastObjc, lastSwift)
        else return (Nothing, Nothing)

parseLastGenLine :: String -> Maybe (OWALanguageType, UTCTime)
parseLastGenLine lastGenLine = case components of
  [typeString, timeString] -> case (typeString, read timeString :: UTCTime) of
    ("Objc", time) -> Just (LanguageTypeObjc, time)
    ("Swift", time) -> Just (LanguageTypeSwift, time) 
    _ -> Nothing
  _ -> Nothing
  where
    components = splitOn ": " lastGenLine

modifyLastGenTime :: FilePath -> OWAReaderT ()
modifyLastGenTime filePath = do
  lang <- languageType <$> ask
  objcTime <- lastObjcGenTime <$> ask
  swiftTime <- lastSwiftGenTime <$> ask
  currentTime <- liftIO getCurrentTime
  let lastGenFilePath = filePath ++ lastGenFileExtension
  liftIO $ case lang of
    LanguageTypeObjc -> do
      let objcString = Just $ "Objc: " ++ show currentTime
      let swiftString = case swiftTime of
                          Nothing -> Nothing
                          Just t -> Just $ "Swift: " ++ show t
      let finalString = unlines (catMaybes [objcString, swiftString])
      writeFile lastGenFilePath finalString
    LanguageTypeSwift -> do
      let swiftString = Just $ "Swift: " ++ show currentTime
      let objcString = case objcTime of
                          Nothing -> Nothing
                          Just t -> Just $ "Objc: " ++ show t
      let finalString = unlines (catMaybes [objcString, swiftString])
      writeFile lastGenFilePath finalString
  
lastGenFileExtension :: FilePath
lastGenFileExtension = "/.owa_last_gen"

appInfoFileExtension :: FilePath
appInfoFileExtension = "/app.info"

shouldRegenerateFromFiles :: [FilePath] -> OWAReaderT Bool
shouldRegenerateFromFiles sourceFiles = do
  info <- ask
  let langType = languageType info 
  let genTime = if langType == LanguageTypeObjc
                  then lastObjcGenTime info
                  else lastSwiftGenTime info
  sourceTimes <- liftIO $ mapM getModificationTime sourceFiles
  case genTime of
    Nothing -> return True
    Just time -> return $ any (time <) sourceTimes

---------------------------------------------------------------------------
------------------------PROGRAM STATUS PRINTING----------------------------
---------------------------------------------------------------------------

data OutputMode = Silent | Normal | Verbose deriving (Show, Eq)

outputModeFromArgs :: [String] -> OutputMode
outputModeFromArgs args 
  | silentMode = Silent
  | verboseMode = Verbose
  | otherwise = Normal 
    where silentMode = elem "-silent" args || elem "-s" args
          verboseMode = elem "-verbose" args || elem "-v" args

printIfNotSilent :: String -> OWAReaderT ()
printIfNotSilent str = do
  (mode, handle) <- grabModeAndHandle
  Control.Monad.when (mode /= Silent) $ liftIO $ hPutStrLn handle str

printIfVerbose :: String -> OWAReaderT ()
printIfVerbose str = do
  (mode, handle) <- grabModeAndHandle
  Control.Monad.when (mode == Verbose) $ liftIO $ hPutStrLn handle str

grabModeAndHandle :: OWAReaderT (OutputMode, Handle)
grabModeAndHandle = do
  info <- ask
  return (outputMode info, outputHandle info)


printErrors :: [OWAParseError] -> OWAReaderT ()
printErrors [] = return ()
printErrors errors = mapM_ (printIfNotSilent . show) errors

owaReadLine :: OWAMaybeT String
owaReadLine = do
  iHandle <- inputHandle <$> ask
  endOfFile <- liftIO $ hIsEOF iHandle
  if endOfFile
    then fail "Hi"
    else liftIO $ hGetLine iHandle

---------------------------------------------------------------------------
------------------------EVALUATING CODE AND LANGUAGE TYPES-----------------
---------------------------------------------------------------------------

allCodeTypes :: [OWACodeType]
allCodeTypes = 
  [ CodeTypeColors
  , CodeTypeFonts
  , CodeTypeAlerts
  , CodeTypeErrors
  , CodeTypeViews
  , CodeTypeStrings ]

codeTypesFromArgs :: [String] -> [OWACodeType]
codeTypesFromArgs args = if null typesInArgs
  then allCodeTypes
  else typesInArgs
  where
    evaluateAndAdd arg accum = case arg of
      "--colors" -> CodeTypeColors : accum
      "--fonts" -> CodeTypeFonts : accum
      "--alerts" -> CodeTypeAlerts : accum
      "--errors" -> CodeTypeErrors : accum
      "--views" -> CodeTypeViews : accum
      "--strings" -> CodeTypeStrings : accum
      _ -> accum
    typesInArgs = foldr evaluateAndAdd [] args

whenCodeTypePresent :: OWACodeType -> OWAReaderT () -> OWAReaderT ()
whenCodeTypePresent codeType action = do
  types <- codeTypes <$> ask
  when (codeType `elem` types) action

languageTypeFromArgs :: [String] -> OWALanguageType
languageTypeFromArgs args = if "--swift" `elem` args
  then LanguageTypeSwift
  else LanguageTypeObjc

---------------------------------------------------------------------------
------------------------LOADING APP INFO-----------------------------------
---------------------------------------------------------------------------

loadAppInfo :: FilePath -> OWAReaderT (Maybe OWAAppInfo)
loadAppInfo appDirectory = do
  printIfNotSilent "Searching for app.info..."
  maybeAppFile <- liftIO $ findAppInfoFile appDirectory
  case maybeAppFile of
    Nothing -> do
      printIfNotSilent "Unable to find app.info. Please create an app.info file"
      return Nothing 
    Just appFile -> do
      printIfVerbose $ "Found app.info at " ++ appFile
      printIfNotSilent "Parsing app.info..."
      appInfoOrErrors <- liftIO $ parseAppInfoFromFile appFile
      case appInfoOrErrors of
        Left errors -> do
          printErrors errors 
          printIfNotSilent "Unable to parse app.info!"
          return Nothing
        Right appInfo -> do
          printIfNotSilent "Successfully parsed app.info!"
          return $ Just appInfo 

---------------------------------------------------------------------------
------------------------PRODUCING STRINGS FILES----------------------------
---------------------------------------------------------------------------

produceStringsFile :: FilePath -> OWAAppInfo -> OWAReaderT ()
produceStringsFile appDirectory appInfo = whenCodeTypePresent CodeTypeStrings $ do
  printIfNotSilent "Generating strings..."
  printIfVerbose "Searching for strings files..."
  stringsFiles <- liftIO $ findStringsFiles appDirectory
  printIfVerbose "Found strings files at: "
  mapM_ printIfVerbose stringsFiles
  regenFiles <- shouldRegenerateFromFiles ((appDirectory ++ appInfoFileExtension) : stringsFiles)
  if not regenFiles
    then do
      printIfNotSilent "No strings modifications since last generate!"
      printIfNotSilent "Skipping strings files generation!"
    else do
      listOfParseResults <- liftIO $ mapM parseStringsFromFile stringsFiles
      let errors = concat $ lefts listOfParseResults
      if not (null errors)
        then do
          printIfNotSilent "Encountered errors parsing strings..."
          printErrors errors
        else printIfVerbose "No errors parsing strings!"
      let stringSets = rights listOfParseResults
      printIfVerbose ("Successfully parsed " ++ show (length stringSets) ++ " sets of strings")
      let stringsFileStructure = objcStringsFileFromStringSets appInfo stringSets
      printIfVerbose "Printing strings file..."
      let fullStringsPath = appDirectory ++ stringsFileExtension
      liftIO $ printStructureToFile stringsFileStructure fullStringsPath
      printIfVerbose "Printed strings to :" 
      printIfVerbose fullStringsPath
      printIfNotSilent "Finished generating strings!"

stringsFileExtension :: String
stringsFileExtension = "/Localizable.strings"

---------------------------------------------------------------------------
------------------------PRODUCING COLORS FILES-----------------------------
---------------------------------------------------------------------------

produceColorsFiles :: FilePath -> OWAAppInfo -> OWAReaderT ()
produceColorsFiles appDirectory appInfo = whenCodeTypePresent CodeTypeColors $ do
  printIfNotSilent "Generating colors..."
  printIfVerbose "Searching for colors files..."
  colorFiles <- liftIO $ findColorsFiles appDirectory
  printIfVerbose "Found colors files at: "
  mapM_ printIfVerbose colorFiles
  regenFiles <- shouldRegenerateFromFiles ((appDirectory ++ appInfoFileExtension) : colorFiles)
  if not regenFiles
    then do
      printIfNotSilent "No colors modifications since last generate!"
      printIfNotSilent "Skipping colors files generation!"
      return ()
    else do
      listOfParseResults <- liftIO $ mapM parseColorsFromFile colorFiles
      let errors = concat $ lefts listOfParseResults 
      if not (null errors)
        then do
          printIfNotSilent "Encountered errors parsing colors..."
          printErrors errors
        else printIfVerbose "No errors parsing colors!"
      let colors = concat $ rights listOfParseResults
      let prefix = appPrefix appInfo
      printIfVerbose ("Successfully parsed " ++ show (length colors) ++ " colors")
      lang <- languageType <$> ask
      producedFiles <- if lang == LanguageTypeObjc
        then do
          let colorHeaderFileStructure = objcHeaderFromColors appInfo colors
          let colorMFileStructure = objcImplementationFromColors appInfo colors
          printIfVerbose "Printing colors files..."
          let fullHeaderPath = appDirectory ++ colorHeaderFileExtension prefix
          let fullMPath = appDirectory ++ colorImplementationFileExtension prefix
          liftIO $ printStructureToFile colorHeaderFileStructure fullHeaderPath
          liftIO $ printStructureToFile colorMFileStructure fullMPath
          return $ fullHeaderPath ++ ", " ++ fullMPath
        else do
          let colorFileStructure = swiftExtensionFromColors appInfo colors
          printIfVerbose "Printing colors files..."
          let fullPath = appDirectory ++ colorSwiftFileExtension prefix
          liftIO $ printSwiftStructureToFile colorFileStructure fullPath
          return fullPath
      printIfVerbose "Printed colors to files:"
      printIfVerbose producedFiles
      printIfNotSilent "Finished generating colors!"

colorHeaderFileExtension :: String -> FilePath
colorHeaderFileExtension prefix = "/UIColor+" ++ prefix ++ "Colors.h"

colorImplementationFileExtension :: String -> FilePath
colorImplementationFileExtension prefix = "/UIColor+" ++ prefix ++ "Colors.m"

colorSwiftFileExtension :: String -> FilePath
colorSwiftFileExtension prefix = "/UIColor+" ++ prefix ++ "Colors.swift"

---------------------------------------------------------------------------
------------------------PRODUCING FONTS FILES------------------------------
---------------------------------------------------------------------------

produceFontsFiles :: FilePath -> OWAAppInfo -> OWAReaderT ()
produceFontsFiles appDirectory appInfo = whenCodeTypePresent CodeTypeFonts $ do
  printIfNotSilent "Generating fonts..."
  printIfVerbose "Searching for fonts files..."
  fontFiles <- liftIO $ findFontsFiles appDirectory
  printIfVerbose "Found fonts files at: "
  mapM_ printIfVerbose fontFiles
  regenFiles <- shouldRegenerateFromFiles ((appDirectory ++ appInfoFileExtension) : fontFiles)
  if not regenFiles
    then do
      printIfNotSilent "No fonts modifications since last generate!"
      printIfNotSilent "Skipping fonts files generation!"
    else do
      listOfParseResults <- liftIO $ mapM parseFontsFromFile fontFiles
      let errors = concat $ lefts listOfParseResults 
      if not (null errors)
        then do
          printIfNotSilent "Encountered errors parsing fonts..."
          printErrors errors
        else printIfVerbose "No errors parsing fonts!"
      let fonts = concat $ rights listOfParseResults 
      let prefix = appPrefix appInfo
      printIfVerbose ("Found " ++ show (length fonts) ++ " fonts")
      lang <- languageType <$> ask
      producedFiles <- if lang == LanguageTypeObjc
        then do
          let fontHeaderFileStructure = objcHeaderFromFonts appInfo fonts
          let fontMFileStructure = objcImplementationFromFonts appInfo fonts
          printIfVerbose "Printing fonts files..."
          let fullHeaderPath = appDirectory ++ fontHeaderFileExtension prefix
          let fullMPath = appDirectory ++ fontImplementationFileExtension prefix
          liftIO $ printStructureToFile fontHeaderFileStructure fullHeaderPath
          liftIO $ printStructureToFile fontMFileStructure fullMPath
          return $ fullHeaderPath ++ ", " ++ fullMPath
        else do
          let fontFileStructure = swiftExtensionFromFonts appInfo fonts
          printIfVerbose "Printing fonts files..."
          let fullPath = appDirectory ++ fontSwiftFileExtension prefix
          liftIO $ printSwiftStructureToFile fontFileStructure fullPath
          return fullPath
      printIfVerbose "Printed fonts to files:"
      printIfVerbose producedFiles 
      printIfNotSilent "Finished generating fonts!"

fontHeaderFileExtension :: String -> FilePath
fontHeaderFileExtension prefix = "/UIFont+" ++ prefix ++ "Fonts.h"

fontImplementationFileExtension :: String -> FilePath
fontImplementationFileExtension prefix = "/UIFont+" ++ prefix ++ "Fonts.m"

fontSwiftFileExtension :: String -> FilePath
fontSwiftFileExtension prefix = "/UIFont+" ++ prefix ++ "Fonts.swift"

---------------------------------------------------------------------------
------------------------PRODUCING ALERTS FILES-----------------------------
---------------------------------------------------------------------------

produceAlertsFiles :: FilePath -> OWAAppInfo -> OWAReaderT ()
produceAlertsFiles appDirectory appInfo = whenCodeTypePresent CodeTypeAlerts $ do
  printIfNotSilent "Generating alerts..."
  printIfVerbose "Searching for alerts files..."
  alertFiles <- liftIO $ findAlertsFiles appDirectory
  printIfVerbose "Found alerts files at: "
  mapM_ printIfVerbose alertFiles
  regenFiles <- shouldRegenerateFromFiles ((appDirectory ++ appInfoFileExtension) : alertFiles)
  if not regenFiles
    then do
      printIfNotSilent "No alerts modifications since last generate!"
      printIfNotSilent "Skipping alerts files generation!"
    else do
      listOfParseResults <- liftIO $ mapM parseAlertsFromFile alertFiles
      let errors = concat $ lefts listOfParseResults 
      if not (null errors)
        then do
          printIfNotSilent "Encountered errors parsing alerts..."
          printErrors errors
        else printIfVerbose "No errors parsing alerts!"
      let alerts = concat $ rights listOfParseResults
      let prefix = appPrefix appInfo
      printIfVerbose ("Found " ++ show (length alerts) ++ " alerts")
      lang <- languageType <$> ask
      producedFiles <- if lang == LanguageTypeObjc
        then do
          let alertHeaderFileStructure = objcHeaderFromAlerts appInfo alerts
          let alertMFileStructure = objcImplementationFromAlerts appInfo alerts
          printIfVerbose "Printing alerts files..."
          let fullHeaderPath = appDirectory ++ alertHeaderFileExtension prefix
          let fullMPath = appDirectory ++ alertImplementationFileExtension prefix
          liftIO $ printStructureToFile alertHeaderFileStructure fullHeaderPath
          liftIO $ printStructureToFile alertMFileStructure fullMPath
          return $ fullHeaderPath ++ ", " ++ fullMPath
        else do
          let alertFileStructure = swiftExtensionFromAlerts appInfo alerts
          printIfVerbose "Printing alerts files..."
          let fullPath = appDirectory ++ alertSwiftFileExtension prefix
          liftIO $ printSwiftStructureToFile alertFileStructure fullPath
          return fullPath
      printIfVerbose "Printed alerts to files:"
      printIfVerbose producedFiles
      printIfNotSilent "Finished generating alerts!"

alertHeaderFileExtension :: String -> FilePath
alertHeaderFileExtension prefix = "/UIAlertController+" ++ prefix ++ "Alerts.h"

alertImplementationFileExtension :: String -> FilePath
alertImplementationFileExtension prefix = "/UIAlertController+" ++ prefix ++ "Alerts.m"

alertSwiftFileExtension :: String -> FilePath
alertSwiftFileExtension prefix = "/UIAlertController+" ++ prefix ++ "Alerts.swift"

---------------------------------------------------------------------------
------------------------PRODUCING ERRORS FILES-----------------------------
---------------------------------------------------------------------------

produceErrorsFiles :: FilePath -> OWAAppInfo -> OWAReaderT ()
produceErrorsFiles appDirectory appInfo = whenCodeTypePresent CodeTypeErrors $ do
  printIfNotSilent "Generating errors..."
  printIfVerbose "Searching for errors files..."
  errorFiles <- liftIO $ findErrorsFiles appDirectory
  printIfVerbose "Found errors files at: "
  mapM_ printIfVerbose errorFiles
  regenFiles <- shouldRegenerateFromFiles ((appDirectory ++ appInfoFileExtension) : errorFiles)
  if not regenFiles
    then do
      printIfNotSilent "No errors modifications since last generate!"
      printIfNotSilent "Skipping errors files generation!"
    else do
      listOfParseResults <- liftIO $ mapM parseErrorsFromFile errorFiles
      let errors = concat $ lefts listOfParseResults 
      if not (null errors)
        then do
          printIfNotSilent "Encountered errors parsing errors..."
          printErrors errors
        else printIfVerbose "No errors parsing errors!"
      let errors = concat $ rights listOfParseResults
      let prefix = appPrefix appInfo
      printIfVerbose ("Found " ++ show (length errors) ++ " errors")
      lang <- languageType <$> ask
      producedFiles <- if lang == LanguageTypeObjc
        then do
          let errorHeaderFileStructure = objcHeaderFromErrors appInfo errors
          let errorMFileStructure = objcImplementationFromErrors appInfo errors
          printIfVerbose "Printing errors files..."
          let fullHeaderPath = appDirectory ++ errorHeaderFileExtension prefix
          let fullMPath = appDirectory ++ errorImplementationFileExtension prefix
          liftIO $ printStructureToFile errorHeaderFileStructure fullHeaderPath
          liftIO $ printStructureToFile errorMFileStructure fullMPath
          return $ fullHeaderPath ++ ", " ++ fullMPath
        else do
          let errorFileStructure = swiftExtensionFromErrors appInfo errors
          printIfVerbose "Printing errors files..."
          let fullPath = appDirectory ++ errorSwiftFileExtension prefix
          liftIO $ printSwiftStructureToFile errorFileStructure fullPath
          return fullPath
      printIfVerbose "Printed errors to files:"
      printIfVerbose producedFiles 
      printIfNotSilent "Finished generating errors!"

errorHeaderFileExtension :: String -> FilePath
errorHeaderFileExtension prefix = "/NSError+" ++ prefix ++ "Errors.h"

errorImplementationFileExtension :: String -> FilePath
errorImplementationFileExtension prefix = "/NSError+" ++ prefix ++ "Errors.m"

errorSwiftFileExtension :: String -> FilePath
errorSwiftFileExtension prefix = "/NSError+" ++ prefix ++ "Errors.swift"

---------------------------------------------------------------------------
------------------------PRODUCING VIEWS FILES------------------------------
---------------------------------------------------------------------------

produceViewsFiles :: FilePath -> OWAAppInfo -> OWAReaderT ()
produceViewsFiles appDirectory appInfo = whenCodeTypePresent CodeTypeViews $ do
  printIfNotSilent "Generating views..."
  printIfVerbose "Searching for views files..."
  viewFiles <- liftIO $ findViewsFiles appDirectory
  printIfVerbose "Found view files at: "
  mapM_ printIfVerbose viewFiles
  let appInfoFile = appDirectory ++ appInfoFileExtension
  viewFilesToRegen <- filterM (\v -> shouldRegenerateFromFiles [appInfoFile, v]) viewFiles
  listOfParseResults <- liftIO $ mapM parseViewFromFile viewFilesToRegen
  let errors = concat $ lefts listOfParseResults
  if not (null errors)
    then do
      printIfNotSilent "Encountered errors parsing views..."
      printErrors errors
    else printIfVerbose "No errors parsing views!"
  let views = rights listOfParseResults
  printIfVerbose ("Found" ++ show (length views) ++ " views")
  if not (null views)
    then printIfVerbose "Printing views..."
    else printIfVerbose "All views are up to date!"
  mapM_ (printViewFiles appDirectory appInfo) views
  printIfNotSilent "Finished generating views!"
   
printViewFiles :: FilePath -> OWAAppInfo -> OWAView -> OWAReaderT ()
printViewFiles appDirectory appInfo view = do
  lang <- languageType <$> ask
  if lang == LanguageTypeObjc
    then do
      liftIO $ printStructureToFile headerStructure headerPath
      liftIO $ printStructureToFile mStructure mPath
      printIfVerbose headerPath
      printIfVerbose mPath
    else do
      liftIO $ printSwiftStructureToFile swiftStructure swiftPath
      printIfVerbose swiftPath
  where 
    headerStructure = objcHeaderFromView appInfo view
    mStructure = objcImplementationFromView appInfo view
    vTy = viewType view
    headerPath = appDirectory ++ '/':vTy ++ ".h"
    mPath = appDirectory ++ '/':vTy ++ ".m"
    swiftStructure = swiftFileFromView appInfo view
    swiftPath = appDirectory ++ '/':vTy ++ ".swift"
