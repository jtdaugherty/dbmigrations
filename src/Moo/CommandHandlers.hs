{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Moo.CommandHandlers where

import Data.String.Conversions (cs, (<>))

import Moo.Core
import Moo.CommandUtils
import Control.Monad ( when, forM_ )
import Data.Maybe ( isJust )
import Control.Monad.Reader ( asks )
import System.Exit ( exitWith, ExitCode(..), exitSuccess )
import qualified Data.Time.Clock as Clock
import Control.Monad.Trans ( liftIO )

import Database.Schema.Migrations.Store hiding (getMigrations)
import Database.Schema.Migrations
import Database.Schema.Migrations.Migration
import Database.Schema.Migrations.Backend

newCommand :: CommandHandler
newCommand storeData = do
  required   <- asks _appRequiredArgs
  store      <- asks _appStore
  linear     <- asks _appLinearMigrations
  timestamp  <- asks _appTimestampFilenames
  timeString <- (<>"_") <$> liftIO getCurrentTimestamp

  let [migrationId] = if timestamp
      then fmap (timeString<>) required
      else required
  noAsk <- _noAsk <$> asks _appOptions

  liftIO $ do
    fullPath <- fullMigrationName store migrationId
    when (isJust $ storeLookup storeData migrationId) $
         do
           putStrLn $ "Migration " <> (show fullPath) ++ " already exists"
           exitWith (ExitFailure 1)

    -- Default behavior: ask for dependencies if linear mode is disabled
    deps <- if linear then (return $ leafMigrations storeData) else
           if noAsk then (return []) else
           do
             putStrLn . cs $ "Selecting dependencies for new \
                        \migration: " <> migrationId
             interactiveAskDeps storeData

    result <- if noAsk then (return True) else
              (confirmCreation migrationId deps)

    case result of
      True -> do
               now <- Clock.getCurrentTime
               status <- createNewMigration store $ (newMigration migrationId) { mDeps = deps
                                                    , mTimestamp = Just now
                                                    }
               case status of
                    Left e -> putStrLn e >> (exitWith (ExitFailure 1))
                    Right _ -> putStrLn $ "Migration created successfully: " ++
                               show fullPath
      False -> do
              putStrLn "Migration creation cancelled."

upgradeCommand :: CommandHandler
upgradeCommand storeData = do
  isTesting <-  _test <$> asks _appOptions
  withBackend $ \backend -> do
        ensureBootstrappedBackend backend >> commitBackend backend
        migrationNames <- missingMigrations backend storeData
        when (null migrationNames) $ do
                           putStrLn "Database is up to date."
                           exitSuccess
        forM_ migrationNames $ \migrationName -> do
            m <- lookupMigration storeData migrationName
            apply m storeData backend False
        case isTesting of
          True -> do
                 rollbackBackend backend
                 putStrLn "Upgrade test successful."
          False -> do
                 commitBackend backend
                 putStrLn "Database successfully upgraded."

upgradeListCommand :: CommandHandler
upgradeListCommand storeData = do
  withBackend $ \backend -> do
        ensureBootstrappedBackend backend >> commitBackend backend
        migrationNames <- missingMigrations backend storeData
        when (null migrationNames) $ do
                               putStrLn "Database is up to date."
                               exitSuccess
        putStrLn "Migrations to install:"
        forM_ migrationNames (putStrLn . cs . ("  " <>))

reinstallCommand :: CommandHandler
reinstallCommand storeData = do
  isTesting <-  _test <$> asks _appOptions
  required <- asks _appRequiredArgs
  let [migrationId] = required

  withBackend $ \backend -> do
      ensureBootstrappedBackend backend >> commitBackend backend
      m <- lookupMigration storeData migrationId

      _ <- revert m storeData backend
      _ <- apply m storeData backend True

      case isTesting of
        False -> do
          commitBackend backend
          putStrLn "Migration successfully reinstalled."
        True -> do
          rollbackBackend backend
          putStrLn "Reinstall test successful."

listCommand :: CommandHandler
listCommand _ = do
  withBackend $ \backend -> do
      ensureBootstrappedBackend backend >> commitBackend backend
      ms <- getMigrations backend
      forM_ ms $ \m ->
          when (not $ m == rootMigrationName) $ putStrLn . cs $ m

applyCommand :: CommandHandler
applyCommand storeData = do
  isTesting <-  _test <$> asks _appOptions
  required  <- asks _appRequiredArgs
  let [migrationId] = required

  withBackend $ \backend -> do
        ensureBootstrappedBackend backend >> commitBackend backend
        m <- lookupMigration storeData migrationId
        _ <- apply m storeData backend True
        case isTesting of
          False -> do
            commitBackend backend
            putStrLn "Successfully applied migrations."
          True -> do
            rollbackBackend backend
            putStrLn "Migration installation test successful."

revertCommand :: CommandHandler
revertCommand storeData = do
  isTesting <-  _test <$> asks _appOptions
  required <- asks _appRequiredArgs
  let [migrationId] = required

  withBackend $ \backend -> do
      ensureBootstrappedBackend backend >> commitBackend backend
      m <- lookupMigration storeData migrationId
      _ <- revert m storeData backend

      case isTesting of
        False -> do
          commitBackend backend
          putStrLn "Successfully reverted migrations."
        True -> do
          rollbackBackend backend
          putStrLn "Migration uninstallation test successful."

testCommand :: CommandHandler
testCommand storeData = do
  required <- asks _appRequiredArgs
  let [migrationId] = required

  withBackend $ \backend -> do
        ensureBootstrappedBackend backend >> commitBackend backend
        m <- lookupMigration storeData migrationId
        migrationNames <- missingMigrations backend storeData
        -- If the migration is already installed, remove it as part of
        -- the test
        when (not $ migrationId `elem` migrationNames) $
             do _ <- revert m storeData backend
                return ()
        applied <- apply m storeData backend True
        forM_ (reverse applied) $ \migration -> do
                             revert migration storeData backend
        rollbackBackend backend
        putStrLn "Successfully tested migrations."
