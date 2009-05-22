{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Schema.Migrations.Backend.Sqlite
    ()
where

import Database.HDBC ( quickQuery, fromSql, SqlValue(SqlString), IConnection(getTables) )
import Database.HDBC.Sqlite3 ( Connection )
import Database.Schema.Migrations.Migration
    ( Migration(..)
    , newMigration
    )
import Database.Schema.Migrations.Backend
    ( Backend(..) )

createSql :: String
createSql = "CREATE TABLE installed_migrations (\
            \migration_id TEXT)"

revertSql :: String
revertSql = "DROP TABLE installed_migrations"

instance Backend Connection IO where
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
      quickQuery conn "INSERT INTO installed_migrations (migration_id) VALUES (?)" [SqlString $ mId m]
      return ()

    revertMigration conn m =
        case mRevert m of
          Nothing -> return ()
          Just query -> quickQuery conn query [] >> return ()

    getMigrations conn = do
      results <- quickQuery conn "SELECT migration_id FROM installed_migrations" []
      return $ map (\(h:_) -> fromSql h) results