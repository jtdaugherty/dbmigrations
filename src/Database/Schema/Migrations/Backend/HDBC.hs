{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Schema.Migrations.Backend.HDBC
    ()
where

import Database.HDBC ( quickQuery, fromSql, toSql, IConnection(getTables, run) )

import Database.Schema.Migrations.Backend
    ( Backend(..) )
import Database.Schema.Migrations.Migration
    ( Migration(..)
    , newMigration
    )

createSql :: String
createSql = "CREATE TABLE installed_migrations (\
            \migration_id TEXT)"

revertSql :: String
revertSql = "DROP TABLE installed_migrations"

instance (IConnection conn) => Backend conn IO where
    isBootstrapped conn = do
      -- if 'installed_migrations' is in the list of tables,
      -- bootstrapping has been performed.
      tables <- getTables conn
      return $ "installed_migrations" `elem` tables

    getBootstrapMigration _ =
        do
          m <- newMigration "root"
          return $ m { mApply = createSql
                     , mRevert = Just revertSql
                     , mDesc = Just "Migration table installation"
                     }

    applyMigration conn m = do
      quickQuery conn (mApply m) []
      quickQuery conn "INSERT INTO installed_migrations (migration_id) VALUES (?)" [toSql $ mId m]
      return ()

    revertMigration conn m = do
        case mRevert m of
          Nothing -> return ()
          Just query -> run conn query [] >> return ()
        -- Remove migration from installed_migrations in either case.
        quickQuery conn "DELETE FROM installed_migrations WHERE migration_id = ?" [toSql $ mId m]
        return ()

    getMigrations conn = do
      results <- quickQuery conn "SELECT migration_id FROM installed_migrations" []
      return $ map (\(h:_) -> fromSql h) results
