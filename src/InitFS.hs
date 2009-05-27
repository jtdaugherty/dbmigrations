{-# LANGUAGE FlexibleContexts #-}
module Main
    ( main )
where

import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(..), exitSuccess )
import System.FilePath ( (</>) )

import Control.Exception ( bracket )

import qualified Data.Map as Map
import Data.Maybe ( catMaybes )
import Control.Monad ( when )
import Data.List ( intercalate )

import Database.HDBC.Sqlite3 ( connectSqlite3, Connection )
import Database.HDBC ( IConnection(commit, disconnect) )

import Database.Schema.Migrations
    ( missingMigrations
    , createNewMigration
    , ensureBootstrappedBackend
    )
import Database.Schema.Migrations.Filesystem
import Database.Schema.Migrations.Dependencies ( dependencies )
import Database.Schema.Migrations.Migration ( Migration(..) )
import Database.Schema.Migrations.Backend ( Backend, getBootstrapMigration, applyMigration )
import Database.Schema.Migrations.Store ( MigrationStore(..), loadMigrations, depGraphFromStore )
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
         let theMigration = Map.lookup migrationId mapping

         withConnection dbPath $ \conn -> do
                 ensureBootstrappedBackend conn >> commit conn
                 case theMigration of
                   Nothing -> do
                     putStrLn $ "No such migration: " ++ migrationId
                     exitWith (ExitFailure 1)
                   Just m -> do
                     depGraph <- depGraphFromStore store
                     graph <- case depGraph of
                                Left e -> do
                                     putStrLn $ "Error analyzing store: " ++ (show e)
                                     exitWith (ExitFailure 1)
                                Right g -> return g

                     -- the list of migrations that need to be
                     -- installed, including the requested migration.
                     let deps = (mId m):(dependencies graph $ mId m)

                     -- the list of available migrations that are not installed.
                     allMissing <- missingMigrations conn mapping

                     -- only the ones that are missing AND in deps are
                     -- the ones we want to install.  Use deps as the
                     -- source to maintain the correct installation
                     -- order.
                     let toInstall = reverse [ e | e <- deps, e `elem` allMissing ]

                     loadedMigrations <- mapM (loadMigration store) toInstall
                     let migrations = catMaybes loadedMigrations
                     when (length migrations == 0) $ do
                                            putStrLn $ "Nothing to do; " ++
                                                         (mId m) ++
                                                         " already installed."
                                            exitSuccess

                     putStrLn "About to apply migrations:"
                     putStrLn $ intercalate "\n" (map mId migrations)

                     mapM_ applyIt migrations
                     commit conn
                     putStrLn $ "Successfully applied migrations."
                         where
                           applyIt it = do
                                     putStr $ "Applying: " ++ (mId it) ++ "... "
                                     applyMigration conn it
                                     putStrLn "done."

    _ -> do
         putStrLn $ "Unrecognized command: " ++ command
         usage
         exitWith (ExitFailure 1)
