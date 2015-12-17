module Moo.Main
    ( mainWithConf
    , Configuration (..)
    , Args
    , usage
    , usageSpecific
    , procArgs
    )
where

import  Control.Monad.Reader (forM_, runReaderT, when)
import  Data.List (intercalate)
import  Database.HDBC (SqlError, catchSql, seErrorMsg)
import  Prelude  hiding (lookup)
import  System.Environment (getProgName)
import  System.Exit (ExitCode (ExitFailure), exitWith)

import  Database.Schema.Migrations.Filesystem (filesystemStore, FilesystemStoreSettings(..))
import  Database.Schema.Migrations.Store
import  Moo.CommandInterface
import  Moo.Core

type Args = [String]

usage :: IO a
usage = do
  progName <- getProgName

  putStrLn $ "Usage: " ++ progName ++ " <command> [args]"
  putStrLn "Environment:"
  putStrLn $ "  " ++ envDatabaseName ++ ": database connection string"
  putStrLn $ "  " ++ envDatabaseType ++ ": database type, one of " ++
           intercalate "," (map fst databaseTypes)
  putStrLn $ "  " ++ envStoreName ++ ": path to migration store"
  putStrLn $ "  " ++ envLinearMigrations ++ ": whether to use linear migrations (defaults to False)"
  putStrLn "Commands:"
  forM_ commands $ \command -> do
          putStrLn $ "  " ++ usageString command
          putStrLn $ "  " ++ _cDescription command
          putStrLn ""

  putStrLn commandOptionUsage
  exitWith (ExitFailure 1)

usageSpecific :: Command -> IO a
usageSpecific command = do
  pn <- getProgName
  putStrLn $ "Usage: " ++ pn ++ " " ++ usageString command
  exitWith (ExitFailure 1)

procArgs :: Args -> IO (Command, CommandOptions, [String])
procArgs args = do
  when (null args) usage

  command <- case findCommand $ head args of
               Nothing -> usage
               Just c -> return c

  (opts, required) <- getCommandArgs $ tail args

  return (command, opts, required)

mainWithConf :: Args -> Configuration -> IO ()
mainWithConf args conf = do
  (command, opts, required) <- procArgs args

  let dbConnStr = _connectionString conf
      dbType = _databaseType conf
      storePathStr = _migrationStorePath conf
      store = filesystemStore $ FSStore { storePath = storePathStr }
      linear = _linearMigrations conf

  if length required < length ( _cRequired command) then
      usageSpecific command
  else do
    loadedStoreData <- loadMigrations store
    case loadedStoreData of
      Left es -> do
        putStrLn "There were errors in the migration store:"
        forM_ es $ \err -> putStrLn $ "  " ++ show err
      Right storeData -> do
        let st = AppState { _appOptions = opts
                          , _appCommand = command
                          , _appRequiredArgs = required
                          , _appOptionalArgs = ["" :: String]
                          , _appDatabaseConnStr = DbConnDescriptor dbConnStr
                          , _appDatabaseType = dbType
                          , _appStore = store
                          , _appStoreData = storeData
                          , _appLinearMigrations = linear
                          , _appTimestampFilenames = _timestampFilenames conf
                          }
        runReaderT (_cHandler command storeData) st `catchSql` reportSqlError

reportSqlError :: SqlError -> IO a
reportSqlError e = do
  putStrLn $ "\n" ++ "A database error occurred: " ++ seErrorMsg e
  exitWith (ExitFailure 1)
