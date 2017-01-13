module Core.Types where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Time.Clock (UTCTime)
import System.IO (Handle)

-- | Reader Monad for the Runner Context
type OWAReaderT = ReaderT OWARunnerContext IO

-- | Wrapper For Maybe
type OWAMaybeT a = MaybeT OWAReaderT a

-- | Contains all important information about generating
-- our code, such as the output most (as garnered from the arguments),
-- the last generation times, the code types, the langauge we're generating,
-- and so on.
data OWARunnerContext = OWARunnerContext 
  { outputMode       :: OutputMode
  , lastObjcGenTime  :: Maybe UTCTime
  , lastSwiftGenTime :: Maybe UTCTime
  , codeTypes        :: [OWACodeType]
  , languageType     :: OWALanguageType
  , inputHandle      :: Handle
  , outputHandle     :: Handle }

-- | Type for how much debugging output we provide.
data OutputMode = 
  Silent | 
  Normal | 
  Verbose 
  deriving (Show, Eq)

-- | Type for the different types of code we can generate.
data OWACodeType = 
  CodeTypeColors |
  CodeTypeFonts |
  CodeTypeAlerts |
  CodeTypeErrors |
  CodeTypeViews |
  CodeTypeStrings
  deriving (Show, Eq)

-- | Type for the different languages we can generate in.
data OWALanguageType =
  LanguageTypeObjc |
  LanguageTypeSwift
  deriving (Show, Eq)
