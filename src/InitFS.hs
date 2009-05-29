{-# LANGUAGE FlexibleContexts #-}
module Main
    ( main )
where

import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(..), exitSuccess )
import System.FilePath ( (</>) )

import Control.Exception ( bracket )

import Data.Maybe ( listToMaybe )
import Data.List ( intercalate )
import qualified Data.Map as Map
import Control.Monad ( when, forM_ )

import Database.HDBC.Sqlite3 ( connectSqlite3, Connection )
import Database.HDBC ( IConnection(commit, disconnect) )

import Database.Schema.Migrations
    ( migrationsToApply
    , createNewMigration
    , ensureBootstrappedBackend
    )
import Database.Schema.Migrations.Filesystem
import Database.Schema.Migrations.Migration ( Migration(..) )
import Database.Schema.Migrations.Backend ( Backend, getBootstrapMigration, applyMigration )
import Database.Schema.Migrations.Store ( MigrationStore(..), loadMigrations )
import Database.Schema.Migrations.Backend.Sqlite()

-- A command has a name, a number of required arguments' labels, a
-- number of optional arguments' labels, and an action to invoke.
data Command = Command { cName :: String
                       , cRequired :: [String]
                       , cOptional :: [String]
                       , cHandler :: CommandHandler
                       }
-- (required arguments, optional arguments) -> IO ()
type CommandHandler = ([String], [String]) -> IO ()

commands :: [Command]
commands = [ Command "init" ["store_path", "db_path"] [] initStore
           , Command "new" ["store_path", "db_path", "migration_name"] [] new
           , Command "apply" ["store_path", "db_path", "migration_name"] [] apply
           ]

withConnection :: FilePath -> (Connection -> IO a) -> IO a
withConnection dbPath act = bracket (connectSqlite3 dbPath) disconnect act

initStore :: CommandHandler
initStore (required, _) = do
  let [fsPath, dbPath] = required
      store = FSStore { storePath = fsPath }
  withConnection dbPath $ \conn -> do
      getBootstrapMigration conn >>= saveMigration store
      putStrLn $ "Filesystem store initialized at " ++ (storePath store)

new :: CommandHandler
new (required, _) = do
  let [fsPath, migrationId] = required
      store = FSStore { storePath = fsPath }
      fullPath = storePath store </> migrationId
  status <- createNewMigration store migrationId
  case status of
    Left e -> putStrLn e >> (exitWith (ExitFailure 1))
    Right _ -> putStrLn $ "Migration created successfully: " ++ (show fullPath)

apply :: CommandHandler
apply (required, _) = do
  let [fsPath, dbPath, migrationId] = required
      store = FSStore { storePath = fsPath }
  mapping <- loadMigrations store

  withConnection dbPath $ \conn ->
      do
        ensureBootstrappedBackend conn >> commit conn

        -- Look up the migration
        let theMigration = Map.lookup migrationId mapping
        m <- case theMigration of
               Nothing -> do
                        putStrLn $ "No such migration: " ++ migrationId
                        exitWith (ExitFailure 1)
               Just m' -> return m'

        -- Get the list of migrations to apply
        toApply' <- migrationsToApply mapping conn m
        toApply <- case toApply' of
                     Left e -> do
                        putStrLn $ "Error: " ++ e
                        exitWith (ExitFailure 1)
                     Right ms -> return ms

        -- Apply them
        when (null toApply) $ do
                        putStrLn $ "Nothing to do; " ++
                                     (mId m) ++
                                     " already installed."
                        exitSuccess

        mapM_ (applyIt conn) toApply
        commit conn
        putStrLn $ "Successfully applied migrations."
            where
              applyIt conn it = do
                        putStr $ "Applying: " ++ (mId it) ++ "... "
                        applyMigration conn it
                        putStrLn "done."

usageString :: Command -> String
usageString command = intercalate " " ((cName command):requiredArgs ++ optionalArgs)
    where
      requiredArgs = map (\s -> "<" ++ s ++ ">") $ cRequired command
      optionalArgs = map (\s -> "[" ++ s ++ "]") $ cOptional command

usage :: IO a
usage = do
  putStrLn $ "Usage: initstore-fs <command> [args]"
  putStrLn "Commands:"
  forM_ commands $ \command -> do
          putStrLn $ "  " ++ usageString command
  exitWith (ExitFailure 1)

usageSpecific :: Command -> IO a
usageSpecific command = do
  putStrLn $ "Usage: initstore-fs " ++ usageString command
  exitWith (ExitFailure 1)

findCommand :: String -> Maybe Command
findCommand name = listToMaybe [ c | c <- commands, cName c == name ]

main :: IO ()
main = do
  allArgs <- getArgs
  when (null allArgs) usage

  let (commandName:args) = allArgs

  command <- case findCommand commandName of
               Nothing -> usage
               Just c -> return c

  if (length args) < (length $ cRequired command) then
      usageSpecific command else
      cHandler command $ splitAt (length $ cRequired command) args