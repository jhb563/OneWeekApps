module Core.Lazy 
  ( lastCodeGenerationTime
  , modifyLastGenTime
  , shouldRegenerateFromFiles )
  where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory (getModificationTime, doesFileExist)

import Core.FileNames
import Core.FileSearch (findAppDirectory)
import Core.Types

-- | Determines the last time we generated Objective C and Swift from the last gen file.
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
          let parsedLines = seq (length lastGenLines) (mapMaybe parseLastGenLine lastGenLines)
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

-- | Modifies the last gen time of the current language type.
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
  
-- | Determines if we should regenerate based on the input file list.
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
