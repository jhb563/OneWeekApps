module OWATerminal 
  ( outputModeFromArgs
  , printIfNotSilent
  , printIfVerbose
  , printErrors
  , owaReadLine 
  , codeTypesFromArgs
  , whenCodeTypePresent
  , languageTypeFromArgs )
  where


import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import System.IO (hIsEOF, hGetLine, hPutStrLn, Handle)

import OWAExecTypes
import OWAParseError

-- | Interprets the arguments to the program, determines what printing mode we're in.
outputModeFromArgs :: [String] -> OutputMode
outputModeFromArgs args 
  | silentMode = Silent
  | verboseMode = Verbose
  | otherwise = Normal 
    where silentMode = elem "-silent" args || elem "-s" args
          verboseMode = elem "-verbose" args || elem "-v" args

-- | Prints to the program's handle if we are not in silent mode.
printIfNotSilent :: String -> OWAReaderT ()
printIfNotSilent str = do
  (mode, handle) <- grabModeAndHandle
  Control.Monad.when (mode /= Silent) $ liftIO $ hPutStrLn handle str

-- | Prints to the program's handle if we are in verbose mode.
printIfVerbose :: String -> OWAReaderT ()
printIfVerbose str = do
  (mode, handle) <- grabModeAndHandle
  Control.Monad.when (mode == Verbose) $ liftIO $ hPutStrLn handle str

-- | Prints a list of errors if they are present
printErrors :: [OWAParseError] -> OWAReaderT ()
printErrors errors = mapM_ (printIfNotSilent . show) errors

-- | Reads a line from the input handle.
owaReadLine :: OWAMaybeT String
owaReadLine = do
  iHandle <- inputHandle <$> ask
  endOfFile <- liftIO $ hIsEOF iHandle
  if endOfFile
    then fail "Reached end of file"
    else liftIO $ hGetLine iHandle

grabModeAndHandle :: OWAReaderT (OutputMode, Handle)
grabModeAndHandle = do
  info <- ask
  return (outputMode info, outputHandle info)

-- | Use the arguments to determine what code types we will generate.
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

-- | Given a Code Type, determine whether or not to perform the action
-- based on what is in our context.
whenCodeTypePresent :: OWACodeType -> OWAReaderT () -> OWAReaderT ()
whenCodeTypePresent codeType action = do
  types <- codeTypes <$> ask
  when (codeType `elem` types) action

-- | Use the arguments to determine what language code we will generate.
languageTypeFromArgs :: [String] -> OWALanguageType
languageTypeFromArgs args = if "--swift" `elem` args
  then LanguageTypeSwift
  else LanguageTypeObjc

allCodeTypes :: [OWACodeType]
allCodeTypes = 
  [ CodeTypeColors
  , CodeTypeFonts
  , CodeTypeAlerts
  , CodeTypeErrors
  , CodeTypeViews
  , CodeTypeStrings ]

