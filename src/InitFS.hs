{-# LANGUAGE FlexibleContexts #-}
module Main
    ( main )
where

import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(..), exitSuccess )
import System.FilePath ( (</>) )

import Control.Exception ( bracket )

import qualified Data.Map as Map
import Control.Monad ( when )

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

initStore :: (Backend b IO) => b -> FilesystemStore -> IO ()
initStore backend store = do
  getBootstrapMigration backend >>= saveMigration store
  putStrLn $ "Filesystem store initialized at " ++ (storePath store)

usage :: IO ()
usage = do
  putStrLn "Usage: initstore-fs <init|new|apply> [...]"

withConnection :: FilePath -> (Connection -> IO a) -> IO a
withConnection dbPath act = bracket (connectSqlite3 dbPath) disconnect act

main :: IO ()
main = do
  (command:args) <- getArgs

  case command of
    "init" -> do
         let [fsPath, dbPath] = args
             store = FSStore { storePath = fsPath }
         withConnection dbPath $ \conn ->
             initStore conn store

    "new" -> do
         let [fsPath, migrationId] = args
             store = FSStore { storePath = fsPath }
             fullPath = storePath store </> migrationId
         status <- createNewMigration store migrationId
         case status of
           Left e -> putStrLn e >> (exitWith (ExitFailure 1))
           Right _ -> putStrLn $ "Migration created successfully: " ++ (show fullPath)

    "apply" -> do
         let [fsPath, dbPath, migrationId] = args
             store = FSStore { storePath = fsPath }
         mapping <- loadMigrations store

         withConnection dbPath $ \conn -> do
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
                 when (length toApply == 0) $ do
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

    _ -> do
         putStrLn $ "Unrecognized command: " ++ command
         usage
         exitWith (ExitFailure 1)
