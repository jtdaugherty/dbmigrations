{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Moo.Core where

import           Control.Monad.Reader                    (ReaderT)
import qualified Data.Configurator                       as C
import           Data.Configurator.Types                 (Config)
import qualified Data.Text                               as T
import           Database.HDBC                           (IConnection)
import           Database.HDBC.PostgreSQL                (connectPostgreSQL)
import           Database.HDBC.Sqlite3                   (connectSqlite3)

import           Database.Schema.Migrations              ()
import           Database.Schema.Migrations.Backend.HDBC ()
import           Database.Schema.Migrations.Filesystem   (FilesystemStore)
import           Database.Schema.Migrations.Store        (StoreData)


-- |The monad in which the application runs.
type AppT a = ReaderT AppState IO a


-- |The type of actions that are invoked to handle specific commands
type CommandHandler = StoreData -> AppT ()

type ShellEnvironment = [(String, String)]

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


data ConfigBackend = forall a. ConfigBackend_ a => ConfigBackend a

-- | Interface for configuration back-ends
class ConfigBackend_ a where
      getConnectionStr      :: a -> IO (Maybe String)
      getDatabaseType       :: a -> IO (Maybe String)
      getMigrationStorePath :: a -> IO (Maybe FilePath)

instance ConfigBackend_ Config where
         getConnectionStr config = C.lookup config $ T.pack envDatabaseName
         getDatabaseType config = C.lookup config $ T.pack envDatabaseType
         getMigrationStorePath config = C.lookup config $ T.pack envStoreName

instance ConfigBackend_ ShellEnvironment where
         getConnectionStr config = return $ lookup envDatabaseName config
         getDatabaseType config =  return $ lookup envDatabaseType config
         getMigrationStorePath config = return $ lookup envStoreName config

instance ConfigBackend_ ConfigBackend where
         getConnectionStr (ConfigBackend config) = getConnectionStr config
         getDatabaseType  (ConfigBackend config) = getDatabaseType config
         getMigrationStorePath (ConfigBackend config) = getMigrationStorePath config


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
