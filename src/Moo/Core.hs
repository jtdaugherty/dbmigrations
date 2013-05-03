{-# LANGUAGE ExistentialQuantification #-}

module Moo.Core where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Reader (ReaderT)
import qualified Data.Configurator as C
import Data.Configurator.Types (Config)
import qualified Data.Text as T
import Database.HDBC (IConnection)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Database.HDBC.Sqlite3 (connectSqlite3)

import Database.Schema.Migrations ()
import Database.Schema.Migrations.Backend.HDBC ()
import Database.Schema.Migrations.Filesystem (FilesystemStore)
import Database.Schema.Migrations.Store (StoreData)


-- |The monad in which the application runs.
type AppT a = ReaderT AppState IO a


-- |The type of actions that are invoked to handle specific commands
type CommandHandler = StoreData -> AppT ()


-- |Application state which can be accessed by any command handler.
data AppState = AppState { _appOptions         :: CommandOptions
                         , _appCommand         :: Command
                         , _appRequiredArgs    :: [String]
                         , _appOptionalArgs    :: [String]
                         , _appStore           :: FilesystemStore
                         , _appDatabaseConnStr :: Maybe DbConnDescriptor
                         , _appDatabaseType    :: Maybe String
                         , _appStoreData       :: StoreData
                         }

type ShellEnvironment = [(String, String)]

data Configuration = Configuration
    { _connectionString   :: Maybe String
    , _databaseType       :: Maybe String
    , _migrationStorePath :: Maybe FilePath
    }

fromShellEnvironment :: ShellEnvironment -> Configuration
fromShellEnvironment env = Configuration connectionString
                                         databaseType
                                         migrationStorePath
    where
      connectionString = envLookup envDatabaseName
      databaseType = envLookup envDatabaseType
      migrationStorePath = envLookup envStoreName
      envLookup = (\evar -> lookup evar env)

fromConfigurator :: Config -> IO Configuration
fromConfigurator conf = Configuration <$> connectionString
                                      <*> databaseType
                                      <*> migrationStorePath
    where
      connectionString = configLookup envDatabaseName
      databaseType = configLookup envDatabaseType
      migrationStorePath = configLookup envStoreName
      configLookup = C.lookup conf . T.pack

-- |Type wrapper for IConnection instances so the makeConnection
-- function can return any type of connection.
data AnyIConnection = forall c. (IConnection c) => AnyIConnection c


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
databaseTypes :: [(String, String -> IO AnyIConnection)]
databaseTypes = [ ("postgresql", fmap AnyIConnection . connectPostgreSQL)
                , ("sqlite3", fmap AnyIConnection . connectSqlite3)
                ]

envDatabaseType :: String
envDatabaseType = "DBM_DATABASE_TYPE"

envDatabaseName :: String
envDatabaseName = "DBM_DATABASE"

envStoreName :: String
envStoreName = "DBM_MIGRATION_STORE"
