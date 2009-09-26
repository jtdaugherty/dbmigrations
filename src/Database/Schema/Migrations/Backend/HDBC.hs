{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Schema.Migrations.Backend.HDBC
    ()
where

import Database.HDBC ( quickQuery', fromSql, toSql, IConnection(getTables, run) )

import Database.Schema.Migrations.Backend
    ( Backend(..) )
import Database.Schema.Migrations.Migration
    ( Migration(..)
    , newMigration
    )

import Control.Applicative ( (<$>) )

migrationTableName :: String
migrationTableName = "installed_migrations"

createSql :: String
createSql = "CREATE TABLE " ++ migrationTableName ++ " (migration_id TEXT)"

revertSql :: String
revertSql = "DROP TABLE " ++ migrationTableName

-- |General Backend instance for all IO-driven HDBC connection
-- implementations.  You can provide a connection-specific instance if
-- need be; this implementation is provided with the hope that you
-- won't /have/ to do that.
instance (IConnection conn) => Backend conn IO where
    isBootstrapped conn = elem migrationTableName <$> getTables conn

    getBootstrapMigration _ =
        do
          m <- newMigration "root"
          return $ m { mApply = createSql
                     , mRevert = Just revertSql
                     , mDesc = Just "Migration table installation"
                     }

    applyMigration conn m = do
      run conn (mApply m) []
      run conn ("INSERT INTO " ++ migrationTableName ++
                " (migration_id) VALUES (?)") [toSql $ mId m]
      return ()

    revertMigration conn m = do
        case mRevert m of
          Nothing -> return ()
          Just query -> run conn query [] >> return ()
        -- Remove migration from installed_migrations in either case.
        run conn ("DELETE FROM " ++ migrationTableName ++
                  " WHERE migration_id = ?") [toSql $ mId m]
        return ()

    getMigrations conn = do
      results <- quickQuery' conn ("SELECT migration_id FROM " ++ migrationTableName) []
      return $ map (\(h:_) -> fromSql h) results
