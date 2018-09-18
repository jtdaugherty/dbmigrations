module Database.Schema.Migrations.Backend.HDBC
    ( hdbcBackend
    )
where

import Database.HDBC
  ( quickQuery'
  , fromSql
  , toSql
  , IConnection(getTables, run, runRaw)
  , commit
  , rollback
  , disconnect
  )

import Database.Schema.Migrations.Backend
    ( Backend(..)
    , rootMigrationName
    )
import Database.Schema.Migrations.Migration
    ( Migration(..)
    , newMigration
    )

import Data.Text ( Text )
import Data.String.Conversions ( cs, (<>) )

import Control.Applicative ( (<$>) )
import Data.Time.Clock (getCurrentTime)

migrationTableName :: Text
migrationTableName = "installed_migrations"

createSql :: Text
createSql = "CREATE TABLE " <> migrationTableName <> " (migration_id TEXT)"

revertSql :: Text
revertSql = "DROP TABLE " <> migrationTableName

-- |General Backend constructor for all HDBC connection implementations.
hdbcBackend :: (IConnection conn) => conn -> Backend
hdbcBackend conn =
    Backend { isBootstrapped = elem (cs migrationTableName) <$> getTables conn
            , getBootstrapMigration =
                  do
                    ts <- getCurrentTime
                    return $ (newMigration rootMigrationName)
                        { mApply = createSql
                        , mRevert = Just revertSql
                        , mDesc = Just "Migration table installation"
                        , mTimestamp = Just ts
                        }

            , applyMigration = \m -> do
                runRaw conn (cs $ mApply m)
                _ <- run conn (cs $ "INSERT INTO " <> migrationTableName <>
                          " (migration_id) VALUES (?)") [toSql $ mId m]
                return ()

            , revertMigration = \m -> do
                  case mRevert m of
                    Nothing -> return ()
                    Just query -> runRaw conn (cs query)
                  -- Remove migration from installed_migrations in either case.
                  _ <- run conn (cs $ "DELETE FROM " <> migrationTableName <>
                            " WHERE migration_id = ?") [toSql $ mId m]
                  return ()

            , getMigrations = do
                results <- quickQuery' conn (cs $ "SELECT migration_id FROM " <> migrationTableName) []
                return $ map (fromSql . head) results

            , commitBackend = commit conn

            , rollbackBackend = rollback conn

            , disconnectBackend = disconnect conn
            }
