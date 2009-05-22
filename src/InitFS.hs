{-# LANGUAGE FlexibleContexts #-}
module Main
    ( main )
where

import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(..) )

import Database.HDBC.Sqlite3 ( connectSqlite3 )

import Database.Schema.Migrations.Filesystem
import Database.Schema.Migrations.Backend ( Backend, getBootstrapMigration )
import Database.Schema.Migrations.Store ( MigrationStore, saveMigration )
import Database.Schema.Migrations.Backend.Sqlite()

initStore :: (Backend b IO) => b -> FilesystemStore -> IO ()
initStore backend store = do
  getBootstrapMigration backend >>= saveMigration store
  putStrLn $ "Filesystem store initialized at " ++ (storePath store)

usage :: IO ()
usage = do
  putStrLn "Usage: initstore-fs <init> [...]"

main :: IO ()
main = do
  (command:args) <- getArgs

  case command of
    "init" -> do
         let [fsPath, dbPath] = args
         store <- newFilesystemStore fsPath
         conn <- connectSqlite3 dbPath
         initStore conn store
    _ -> do
         putStrLn $ "Unrecognized command: " ++ command
         usage
         exitWith (ExitFailure 1)
