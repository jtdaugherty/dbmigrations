{-# LANGUAGE ExistentialQuantification #-}
module Moo.Core where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Reader (ReaderT)
import qualified Data.Configurator as C
import Data.Configurator.Types (Config)
import qualified Data.Text as T
import Data.Char (toLower)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Database.HDBC.Sqlite3 (connectSqlite3)
import System.Environment (getEnvironment)

import Database.Schema.Migrations ()
import Database.Schema.Migrations.Store (MigrationStore, StoreData)
import Database.Schema.Migrations.Backend
import Database.Schema.Migrations.Backend.HDBC

-- |The monad in which the application runs.
type AppT a = ReaderT AppState IO a

-- |The type of actions that are invoked to handle specific commands
type CommandHandler = StoreData -> AppT ()

-- |Application state which can be accessed by any command handler.
data AppState = AppState { _appOptions          :: CommandOptions
                         , _appCommand          :: Command
                         , _appRequiredArgs     :: [String]
                         , _appOptionalArgs     :: [String]
                         , _appStore            :: MigrationStore
                         , _appDatabaseConnStr  :: DbConnDescriptor
                         , _appDatabaseType     :: String
                         , _appStoreData        :: StoreData
                         , _appLinearMigrations :: Bool
                         }

type ShellEnvironment = [(String, String)]

data Configuration = Configuration
    { _connectionString   :: String
    , _databaseType       :: String
    , _migrationStorePath :: FilePath
    , _linearMigrations   :: Bool
    }

loadConfiguration :: Maybe FilePath -> IO (Either String Configuration)
loadConfiguration pth = do
    mCfg <- case pth of
        Nothing -> fromShellEnvironment <$> getEnvironment
        Just path -> fromConfigurator =<< C.load [C.Required path]

    case mCfg of
        Nothing -> do
            case pth of
                Nothing -> return $ Left "Missing required environment variables"
                Just path -> return $ Left $ "Could not load configuration from " ++ path
        Just cfg -> return $ Right cfg

fromShellEnvironment :: ShellEnvironment -> Maybe Configuration
fromShellEnvironment env = Configuration <$> connectionString
                                         <*> databaseType
                                         <*> migrationStorePath
                                         <*> Just $ linearMigrations
    where
      connectionString = envLookup envDatabaseName
      databaseType = envLookup envDatabaseType
      migrationStorePath = envLookup envStoreName
      linearMigrations = readFlagDef $ envLookup envLinearMigrations
      envLookup = (\evar -> lookup evar env)

-- |Converts @Just "on"@ and @Just "true"@ (case insensitive) to @True@,
-- anything else to @False@.
readFlagDef :: Maybe String -> Bool
readFlagDef Nothing  = False
readFlagDef (Just v) = go $ map toLower v
    where
        go "on"   = True
        go "true" = True
        go _      = False

fromConfigurator :: Config -> IO (Maybe Configuration)
fromConfigurator conf = do
    let configLookup = C.lookup conf . T.pack
    let defConfigLookup d = C.lookupDefault d conf . T.pack
    connectionString <- configLookup envDatabaseName
    databaseType <- configLookup envDatabaseType
    migrationStorePath <- configLookup envStoreName
    linearMigrations <- defConfigLookup False envLinearMigrations

    return $ Configuration <$> connectionString
                           <*> databaseType
                           <*> migrationStorePath
                           <*> Just linearMigrations

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

-- |ConfigOptions are those options read from configuration file
data ConfigData = ConfigData { _dbTypeStr     :: String
                             , _dbConnStr     :: String
                             , _fileStorePath :: String
                             }

newtype DbConnDescriptor = DbConnDescriptor String

-- |The values of DBM_DATABASE_TYPE and their corresponding connection
-- factory functions.
databaseTypes :: [(String, String -> IO Backend)]
databaseTypes = [ ("postgresql", fmap hdbcBackend . connectPostgreSQL)
                , ("sqlite3", fmap hdbcBackend . connectSqlite3)
                ]

envDatabaseType :: String
envDatabaseType = "DBM_DATABASE_TYPE"

envDatabaseName :: String
envDatabaseName = "DBM_DATABASE"

envStoreName :: String
envStoreName = "DBM_MIGRATION_STORE"

envLinearMigrations :: String
envLinearMigrations = "DBM_LINEAR_MIGRATIONS"
