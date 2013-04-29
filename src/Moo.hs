module Main
    ( main )
where

import           Control.Monad                         (liftM)
import           Control.Monad.Reader                  (forM_, runReaderT, when)
import           Data.Configurator
import           Data.List                             (intercalate)
import           Data.Maybe                            (fromMaybe)
import           Database.HDBC                         (SqlError, catchSql,
                                                        seErrorMsg)
import           Prelude                               hiding (lookup)
import           System.Environment                    (getArgs, getEnvironment,
                                                        getProgName)
import           System.Exit                           (ExitCode (ExitFailure),
                                                        exitWith)

import           Data.Maybe
import           Database.Schema.Migrations.Filesystem (FilesystemStore (..))
import           Database.Schema.Migrations.Store
import           Moo.CommandInterface
import           Moo.Core

reportSqlError :: SqlError -> IO a
reportSqlError e = do
  putStrLn $ "\n" ++ "A database error occurred: " ++ seErrorMsg e
  exitWith (ExitFailure 1)


usage :: IO a
usage = do
  progName <- getProgName

  putStrLn $ "Usage: " ++ progName ++ " <command> [args]"
  putStrLn "Environment:"
  putStrLn $ "  " ++ envDatabaseName ++ ": database connection string"
  putStrLn $ "  " ++ envDatabaseType ++ ": database type, one of " ++
           intercalate "," (map fst databaseTypes)
  putStrLn $ "  " ++ envStoreName ++ ": path to migration store"
  putStrLn "Commands:"
  forM_ commands $ \command -> do
          putStrLn $ "  " ++ usageString command
          putStrLn $ "  " ++ _cDescription command
          putStrLn ""

  putStrLn commandOptionUsage
  exitWith (ExitFailure 1)


usageSpecific :: Command -> IO a
usageSpecific command = do
  putStrLn $ "Usage: initstore-fs " ++ usageString command
  exitWith (ExitFailure 1)

loadConfiguration :: Maybe FilePath -> IO Configuration
loadConfiguration Nothing     = liftM fromShellEnvironment getEnvironment
loadConfiguration (Just path) = fromConfigurator =<< load [Required path]

main :: IO ()
main = do
  allArgs <- getArgs
  when (null allArgs) usage
  let (commandName:unprocessedArgs) = allArgs

  command <- case findCommand commandName of
               Nothing -> usage
               Just c  -> return c

  (opts, required) <- getCommandArgs unprocessedArgs

  let optionalConfigPath = _configFilePath opts

  conf <- loadConfiguration optionalConfigPath
  let mDbConnStr = _connectionString conf
  let mDbType    =_databaseType conf
  let mStoreName = _migrationStorePath conf

  let  storePathStr =
         fromMaybe (error $ "Error: missing required environment variable " ++ envStoreName)
                   mStoreName

  let  store = FSStore { storePath = storePathStr }

  if length required < length ( _cRequired command) then
      usageSpecific command else
      do
        loadedStoreData <- loadMigrations store
        case loadedStoreData of
          Left es -> do
            putStrLn "There were errors in the migration store:"
            forM_ es $ \err -> putStrLn $ "  " ++ show err
          Right storeData -> do
            let st = AppState { _appOptions         = opts
                              , _appCommand         = command
                              , _appRequiredArgs    = required
                              , _appOptionalArgs    = ["" :: String]
                              , _appDatabaseConnStr = liftM DbConnDescriptor mDbConnStr
                              , _appDatabaseType    = mDbType
                              , _appStore           = FSStore storePathStr
                              , _appStoreData       = storeData
                              }
            runReaderT (_cHandler command storeData) st `catchSql` reportSqlError
