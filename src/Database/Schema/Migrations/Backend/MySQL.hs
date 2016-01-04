module Database.Schema.Migrations.Backend.MySQL (mysqlBackend) where

import Control.Monad (when)
import Database.MySQL.Simple
import Database.Schema.Migrations.Backend
       (Backend(..), rootMigrationName)
import Database.Schema.Migrations.Migration
       (Migration(..), newMigration)
import Data.Time.Clock (getCurrentTime)
import Data.String (fromString)
import Data.Maybe (listToMaybe)
import qualified Database.MySQL.Base as Base

mysqlBackend :: Connection -> Backend
mysqlBackend conn =
  Backend {isBootstrapped =
             fmap ((Just migrationTableName ==) . listToMaybe . fmap fromOnly)
                  (query conn
                         (fromString "SELECT table_name FROM information_schema.tables WHERE table_name = ? AND table_schema = database()")
                         (Only migrationTableName) :: IO [Only String])
          ,getBootstrapMigration =
             do ts <- getCurrentTime
                return ((newMigration rootMigrationName) {mApply = createSql
                                                         ,mRevert =
                                                            Just revertSql
                                                         ,mDesc =
                                                            Just "Migration table installation"
                                                         ,mTimestamp = Just ts})
          ,applyMigration =
             \m ->
               do execute_ conn (fromString (mApply m))
                  discardResults conn
                  execute conn
                          (fromString
                             ("INSERT INTO " ++
                              migrationTableName ++
                              " (migration_id) VALUES (?)"))
                          (Only (mId m))
                  return ()
          ,revertMigration =
             \m ->
               do case mRevert m of
                    Nothing -> return ()
                    Just sql ->
                      do execute_ conn (fromString sql)
                         return ()
                  discardResults conn
                  -- Remove migration from installed_migrations in either case.
                  execute
                    conn
                    (fromString
                       ("DELETE FROM " ++
                        migrationTableName ++ " WHERE migration_id = ?"))
                    (Only (mId m))
                  return ()
          ,getMigrations =
             do results <-
                  query_ conn
                         (fromString
                            ("SELECT migration_id FROM " ++ migrationTableName))
                return (map fromOnly results)
          ,commitBackend = commit conn
          ,rollbackBackend = rollback conn
          ,disconnectBackend = close conn}

discardResults :: Connection -> IO ()
discardResults conn =
  do more <- Base.nextResult conn
     when more (discardResults conn)

migrationTableName :: String
migrationTableName = "installed_migrations"

createSql :: String
createSql = "CREATE TABLE " ++ migrationTableName ++ " (migration_id TEXT)"

revertSql :: String
revertSql = "DROP TABLE " ++ migrationTableName
