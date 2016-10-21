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
    , DatabaseType
    , rootMigrationName
    )
import Database.Schema.Migrations.Migration
    ( Migration(..)
    , newMigration
    )

import Control.Applicative ( (<$>) )
import Data.Time.Clock (getCurrentTime)

migrationTableName :: String
migrationTableName = "installed_migrations"

createSql :: String
createSql = "CREATE TABLE " ++ migrationTableName ++ " (migration_id TEXT)"

revertSql :: String
revertSql = "DROP TABLE " ++ migrationTableName

-- |General Backend constructor for all HDBC connection implementations.
hdbcBackend :: (IConnection conn) => DatabaseType -> conn -> Backend
hdbcBackend databaseType conn =
    Backend { getType = databaseType
            , isBootstrapped = elem migrationTableName <$> getTables conn
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
                runRaw conn (mApply m)
                run conn ("INSERT INTO " ++ migrationTableName ++
                          " (migration_id) VALUES (?)") [toSql $ mId m]
                return ()

            , revertMigration = \m -> do
                  case mRevert m of
                    Nothing -> return ()
                    Just query -> runRaw conn query
                  -- Remove migration from installed_migrations in either case.
                  run conn ("DELETE FROM " ++ migrationTableName ++
                            " WHERE migration_id = ?") [toSql $ mId m]
                  return ()

            , getMigrations = do
                results <- quickQuery' conn ("SELECT migration_id FROM " ++ migrationTableName) []
                return $ map (fromSql . head) results

            , commitBackend = commit conn

            , rollbackBackend = rollback conn

            , disconnectBackend = disconnect conn
            }
