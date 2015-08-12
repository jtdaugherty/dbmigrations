{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Moo.CommandHandlers where

import Control.Applicative ((<$>))
import Moo.Core
import Moo.CommandUtils
import Control.Monad ( when, forM_ )
import Data.Maybe ( isJust )
import Control.Monad.Reader ( asks )
import System.Exit ( exitWith, ExitCode(..), exitSuccess )
import Control.Monad.Trans ( liftIO )
import Database.HDBC ( IConnection(commit, rollback))

import Database.Schema.Migrations.Store hiding (getMigrations)
import Database.Schema.Migrations
import Database.Schema.Migrations.Backend

newCommand :: CommandHandler
newCommand storeData = do
  required <- asks _appRequiredArgs
  store    <- asks _appStore
  let [migrationId] = required
  noAsk <- _noAsk <$> asks _appOptions

  liftIO $ do
    fullPath <- fullMigrationName store migrationId

    when (isJust $ storeLookup storeData migrationId) $
         do
           putStrLn $ "Migration " ++ (show fullPath) ++ " already exists"
           exitWith (ExitFailure 1)

    -- Default behavior: ask for dependencies
    deps <- if noAsk then (return []) else
            do
              putStrLn $ "Selecting dependencies for new \
                         \migration: " ++ migrationId
              interactiveAskDeps storeData

    result <- if noAsk then (return True) else
              (confirmCreation migrationId deps)

    case result of
      True -> do
               status <- createNewMigration store migrationId deps
               case status of
                 Left e -> putStrLn e >> (exitWith (ExitFailure 1))
                 Right _ -> putStrLn $ "Migration created successfully: " ++
                            show fullPath
      False -> do
               putStrLn "Migration creation cancelled."

upgradeCommand :: CommandHandler
upgradeCommand storeData = do
  isTesting <-  _test <$> asks _appOptions
  withConnection $ \(AnyIConnection conn) -> do
        ensureBootstrappedBackend conn >> commit conn
        migrationNames <- missingMigrations conn storeData
        when (null migrationNames) $ do
                           putStrLn "Database is up to date."
                           exitSuccess
        forM_ migrationNames $ \migrationName -> do
            m <- lookupMigration storeData migrationName
            apply m storeData conn False
        case isTesting of
          True -> do
                 rollback conn
                 putStrLn "Upgrade test successful."
          False -> do
                 commit conn
                 putStrLn "Database successfully upgraded."

upgradeListCommand :: CommandHandler
upgradeListCommand storeData = do
  withConnection $ \(AnyIConnection conn) -> do
        ensureBootstrappedBackend conn >> commit conn
        migrationNames <- missingMigrations conn storeData
        when (null migrationNames) $ do
                               putStrLn "Database is up to date."
                               exitSuccess
        putStrLn "Migrations to install:"
        forM_ migrationNames (putStrLn . ("  " ++))

reinstallCommand :: CommandHandler
reinstallCommand storeData = do
  isTesting <-  _test <$> asks _appOptions
  required <- asks _appRequiredArgs
  let [migrationId] = required

  withConnection $ \(AnyIConnection conn) -> do
      ensureBootstrappedBackend conn >> commit conn
      m <- lookupMigration storeData migrationId

      revert m storeData conn
      apply m storeData conn True

      case isTesting of
        False -> do
          commit conn
          putStrLn "Migration successfully reinstalled."
        True -> do
          rollback conn
          putStrLn "Reinstall test successful."

listCommand :: CommandHandler
listCommand _ = do
  withConnection $ \(AnyIConnection conn) -> do
      ensureBootstrappedBackend conn >> commit conn
      ms <- getMigrations conn
      forM_ ms $ \m ->
          when (not $ m == rootMigrationName) $ putStrLn m

applyCommand :: CommandHandler
applyCommand storeData = do
  isTesting <-  _test <$> asks _appOptions
  required  <- asks _appRequiredArgs
  let [migrationId] = required

  withConnection $ \(AnyIConnection conn) -> do
        ensureBootstrappedBackend conn >> commit conn
        m <- lookupMigration storeData migrationId
        apply m storeData conn True
        case isTesting of
          False -> do
            commit conn
            putStrLn "Successfully applied migrations."
          True -> do
            rollback conn
            putStrLn "Migration installation test successful."

revertCommand :: CommandHandler
revertCommand storeData = do
  isTesting <-  _test <$> asks _appOptions
  required <- asks _appRequiredArgs
  let [migrationId] = required

  withConnection $ \(AnyIConnection conn) -> do
      ensureBootstrappedBackend conn >> commit conn
      m <- lookupMigration storeData migrationId
      revert m storeData conn

      case isTesting of
        False -> do
          commit conn
          putStrLn "Successfully reverted migrations."
        True -> do
          rollback conn
          putStrLn "Migration uninstallation test successful."

testCommand :: CommandHandler
testCommand storeData = do
  required <- asks _appRequiredArgs
  let [migrationId] = required

  withConnection $ \(AnyIConnection conn) -> do
        ensureBootstrappedBackend conn >> commit conn
        m <- lookupMigration storeData migrationId
        migrationNames <- missingMigrations conn storeData
        -- If the migration is already installed, remove it as part of
        -- the test
        when (not $ migrationId `elem` migrationNames) $
             do revert m storeData conn
                return ()
        applied <- apply m storeData conn True
        forM_ (reverse applied) $ \migration -> do
                             revert migration storeData conn
        rollback conn
        putStrLn "Successfully tested migrations."
