module Database.Schema.Migrations.Backend.Sqlite
    ( SqliteBackend(..)
    )
where

import Database.HDBC.Sqlite ( Connection )
import Database.Schema.Migrations.Migration
    ( Migration(..)
    , MigrationID
    , newMigration
    )
import Database.Schema.Migrations.Backend
    ( Backend(..) )

createSql = "CREATE TABLE installed_migrations (\
               migration_id VARCHAR(255))"
revertSql = "DROP TABLE installed_migrations"

instance Backend Connection where
    getBootstrapMigration conn =
        do
          m <- newMigration "root"
          return $ m { mApply = createSql
                     , mRevert = Just revertSql
                     , mDesc = "Migration table installation"
                     }

    applyMigration conn m = do
      undefined

    revertMigration conn m = do
      undefined

    getMigrations conn = do
      undefined