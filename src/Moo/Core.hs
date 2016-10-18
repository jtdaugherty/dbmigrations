{-# LANGUAGE ExistentialQuantification #-}
module Moo.Core
    ( AppT
    , CommandHandler
    , CommandOptions (..)
    , Command (..)
    , AppState (..)
    , Configuration (..)
    , makeParameters
    , ExecutableParameters (..)
    , envDatabaseName
    , envLinearMigrations
    , envStoreName
    , loadConfiguration) where

import Control.Applicative
import Control.Monad.Reader (ReaderT)
import qualified Data.Configurator as C
import Data.Configurator.Types (Config, Configured)
import qualified Data.Text as T
import Data.Char (toLower)
import System.Environment (getEnvironment)
import Data.Maybe (fromMaybe)

import Database.Schema.Migrations.Store (MigrationStore, StoreData)
import Database.Schema.Migrations.Backend

-- |The monad in which the application runs.
type AppT a = ReaderT AppState IO a

-- |The type of actions that are invoked to handle specific commands
type CommandHandler = StoreData -> AppT ()

-- |Application state which can be accessed by any command handler.
data AppState = AppState { _appOptions          :: CommandOptions
                         , _appCommand          :: Command
                         , _appRequiredArgs     :: [String]
                         , _appOptionalArgs     :: [String]
                         , _appBackend          :: Backend
                         , _appStore            :: MigrationStore
                         , _appStoreData        :: StoreData
                         , _appLinearMigrations :: Bool
                         , _appTimestampFilenames :: Bool
                         }

type ShellEnvironment = [(String, String)]

-- |Intermediate type used during config loading.
data LoadConfig = LoadConfig
    { _lcConnectionString   :: Maybe String
    , _lcMigrationStorePath :: Maybe FilePath
    , _lcLinearMigrations   :: Maybe Bool
    , _lcTimestampFilenames :: Maybe Bool
    } deriving Show

-- |Loading the configuration from a file or having it specified via environment
-- |variables results in a value of type Configuration.
data Configuration = Configuration
    { _connectionString   :: String
    , _migrationStorePath :: FilePath
    , _linearMigrations   :: Bool
    , _timestampFilenames :: Bool
    } deriving Show

-- |A value of type ExecutableParameters is what a moo executable (moo-postgresql,
-- |moo-mysql, etc.) pass to the core package when they want to execute a
-- |command.
data ExecutableParameters = ExecutableParameters
    { _parametersBackend            :: Backend
    , _parametersMigrationStorePath :: FilePath
    , _parametersLinearMigrations   :: Bool
    , _parametersTimestampFilenames :: Bool
    } deriving Show

defConfigFile :: String
defConfigFile = "moo.cfg"

newLoadConfig :: LoadConfig
newLoadConfig = LoadConfig Nothing Nothing Nothing Nothing

validateLoadConfig :: LoadConfig -> Either String Configuration
validateLoadConfig (LoadConfig Nothing _ _ _) =
    Left "Invalid configuration: connection string not specified"
validateLoadConfig (LoadConfig _ Nothing _ _) =
    Left "Invalid configuration: migration store path not specified"
validateLoadConfig (LoadConfig (Just cs) (Just msp) lm ts) =
    Right $ Configuration cs msp (fromMaybe False lm) (fromMaybe False ts)

-- |Setters for fields of 'LoadConfig'.
lcConnectionString, lcMigrationStorePath
    :: LoadConfig -> Maybe String -> LoadConfig
lcConnectionString c v   = c { _lcConnectionString   = v }
lcMigrationStorePath c v = c { _lcMigrationStorePath = v }

lcLinearMigrations :: LoadConfig -> Maybe Bool -> LoadConfig
lcLinearMigrations c v   = c { _lcLinearMigrations   = v }

lcTimestampFilenames :: LoadConfig -> Maybe Bool -> LoadConfig
lcTimestampFilenames c v = c { _lcTimestampFilenames = v }


-- | @f .= v@ invokes f only if v is 'Just'
(.=) :: (Monad m) => (a -> Maybe b -> a) -> m (Maybe b) -> m (a -> a)
(.=) f v' = do
    v <- v'
    return $ case v of
      Just _ -> flip f v
      _      -> id

-- |It's just @flip '<*>'@
(&) :: (Applicative m) => m a -> m (a -> b) -> m b
(&) = flip (<*>)

infixr 3 .=
infixl 2 &

applyEnvironment :: ShellEnvironment -> LoadConfig -> IO LoadConfig
applyEnvironment env lc =
    return lc & lcConnectionString   .= f envDatabaseName
              & lcMigrationStorePath .= f envStoreName
              & lcLinearMigrations   .= readFlag <$> f envLinearMigrations
              & lcTimestampFilenames .= readFlag <$> f envTimestampFilenames
    where f n = return $ lookup n env

applyConfigFile :: Config -> LoadConfig -> IO LoadConfig
applyConfigFile cfg lc =
    return lc & lcConnectionString   .= f envDatabaseName
              & lcMigrationStorePath .= f envStoreName
              & lcLinearMigrations   .= f envLinearMigrations
              & lcTimestampFilenames .= f envTimestampFilenames
    where
        f :: Configured a => String -> IO (Maybe a)
        f = C.lookup cfg . T.pack

-- |Loads config file (falling back to default one if not specified) and then
-- overrides configuration with an environment.
loadConfiguration :: Maybe FilePath -> IO (Either String Configuration)
loadConfiguration pth = do
    file <- maybe (C.load [C.Optional defConfigFile])
                  (\p -> C.load [C.Required p]) pth
    env <- getEnvironment
    cfg <- applyConfigFile file newLoadConfig >>= applyEnvironment env

    return $ validateLoadConfig cfg

makeParameters :: Configuration -> Backend -> ExecutableParameters
makeParameters conf backend =
   ExecutableParameters
    { _parametersBackend            = backend
    , _parametersMigrationStorePath = _migrationStorePath conf
    , _parametersLinearMigrations   = _linearMigrations   conf
    , _parametersTimestampFilenames = _timestampFilenames conf
    }

-- |Converts @Just "on"@ and @Just "true"@ (case insensitive) to @True@,
-- anything else to @False@.
readFlag :: Maybe String -> Maybe Bool
readFlag Nothing  = Nothing
readFlag (Just v) = go $ map toLower v
    where
        go "on"    = Just True
        go "true"  = Just True
        go "off"   = Just False
        go "false" = Just False
        go _       = Nothing

-- |CommandOptions are those options that can be specified at the command
-- prompt to modify the behavior of a command.
data CommandOptions = CommandOptions { _configFilePath :: Maybe String
                                     , _test           :: Bool
                                     , _noAsk          :: Bool
                                     }

-- |A command has a name, a number of required arguments' labels, a
-- number of optional arguments' labels, and an action to invoke.
data Command = Command { _cName           :: String
                       , _cRequired       :: [String]
                       , _cOptional       :: [String]
                       , _cAllowedOptions :: [String]
                       , _cDescription    :: String
                       , _cHandler        :: CommandHandler
                       }

envDatabaseName :: String
envDatabaseName = "DBM_DATABASE"

envStoreName :: String
envStoreName = "DBM_MIGRATION_STORE"

envLinearMigrations :: String
envLinearMigrations = "DBM_LINEAR_MIGRATIONS"

envTimestampFilenames :: String
envTimestampFilenames = "DBM_TIMESTAMP_FILENAMES"

