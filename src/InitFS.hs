{-# LANGUAGE FlexibleContexts #-}
module Main
    ( main )
where

import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(..) )

import Control.Exception ( bracket )

import qualified Data.Map as Map

import Database.HDBC.Sqlite3 ( connectSqlite3, Connection )
import Database.HDBC ( IConnection(commit, disconnect) )

import Database.Schema.Migrations.Filesystem
import Database.Schema.Migrations.Backend ( Backend, getBootstrapMigration, applyMigration )
import Database.Schema.Migrations.Store ( MigrationStore, saveMigration, loadMigrations )
import Database.Schema.Migrations.Backend.Sqlite()

initStore :: (Backend b IO) => b -> FilesystemStore -> IO ()
initStore backend store = do
  getBootstrapMigration backend >>= saveMigration store
  putStrLn $ "Filesystem store initialized at " ++ (storePath store)

usage :: IO ()
usage = do
  putStrLn "Usage: initstore-fs <init> [...]"

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

    "apply" -> do
         let [fsPath, dbPath, migrationId] = args
             store = FSStore { storePath = fsPath }
         mapping <- loadMigrations store
         let theMigration = Map.lookup migrationId mapping

         withConnection dbPath $ \conn -> do
                 case theMigration of
                   Nothing -> do
                     putStrLn $ "No such migration: " ++ migrationId
                     exitWith (ExitFailure 1)
                   Just m -> do
                     applyMigration conn m
                     commit conn
                     putStrLn $ "Successfully applied migration: " ++ migrationId

    _ -> do
         putStrLn $ "Unrecognized command: " ++ command
         usage
         exitWith (ExitFailure 1)
