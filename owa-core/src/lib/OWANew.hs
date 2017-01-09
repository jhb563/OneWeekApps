module OWANew (runNewCommand) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Char (toUpper, isAlpha)
import Data.Maybe (isJust, fromJust)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (utctDay, UTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.IO (hClose, hPutStrLn, openFile, IOMode(..))

import Model.OWAAppInfo
import OWAExecTypes
import OWAFileNames
import OWATerminal
import OWAXCode (printBaseXCodeFiles)

-- | Reads information froms the user about creating a project,
-- and causes the XCode files to be generated for that new project, as well
-- as the app.info file.
runNewCommand :: FilePath -> OWAReaderT ()
runNewCommand filePath = do
    (printIfNotSilent "Creating new OWA project!")
    maybeAppInfo <- appInfoReaderAction 
    case maybeAppInfo of
      Nothing -> return ()
      Just appInfo -> do
        liftIO $ createAppInfo appInfo
        liftIO $ printBaseXCodeFiles filePath appInfo
        (printIfNotSilent ("Your new app \"" ++ appName appInfo ++ "\" has been created!")) 
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

