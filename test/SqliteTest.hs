module SqliteTest
    ( tests
    )
where

import Test.HUnit

import Database.Schema.Migrations.Backend.Sqlite ()
import Database.Schema.Migrations.Migration ( Migration(..), newMigration )
import Database.Schema.Migrations.Backend ( Backend(..) )

import Database.HDBC.Sqlite3 ( Connection, connectSqlite3 )
import Database.HDBC ( IConnection(..), catchSql, withTransaction )

tests :: IO [Test]
tests = sequence [ bootstrapTest
                 , applyMigrationFailure
                 , isBootstrappedTrueTest
                 , isBootstrappedFalseTest
                 , revertMigrationFailure
                 , revertMigrationNothing
                 , revertMigrationJust
                 ]

withTempDb :: (Connection -> IO Test) -> IO Test
withTempDb act = connectSqlite3 ":memory:" >>= act

withBootstrappedTempDb :: (Connection -> IO Test) -> IO Test
withBootstrappedTempDb act = do
  conn <- connectSqlite3 ":memory:"
  bs <- getBootstrapMigration conn
  applyMigration conn bs
  commit conn
  act conn

bootstrapTest :: IO Test
bootstrapTest = do
    withTempDb $ \conn -> do
      bs <- getBootstrapMigration conn
      applyMigration conn bs
      tables <- getTables conn
      installed <- getMigrations conn
      return $ test $ [ "successfully bootstrapped" ~:
                          [mId bs] ~=? installed
                      , "installed_migrations table exists" ~:
                          ["installed_migrations"] ~=? tables ]

isBootstrappedTrueTest :: IO Test
isBootstrappedTrueTest = do
  withBootstrappedTempDb $ \conn -> do
    result <- isBootstrapped conn
    return $ test $ True ~=? result

isBootstrappedFalseTest :: IO Test
isBootstrappedFalseTest = do
  withTempDb $ \conn -> do
    result <- isBootstrapped conn
    return $ test $ False ~=? result

ignoreSqlExceptions :: IO a -> IO (Maybe a)
ignoreSqlExceptions act = (act >>= return . Just) `catchSql`
                       (\_ -> return Nothing)

-- |Does a failure to apply a migration imply a transaction rollback?
applyMigrationFailure :: IO Test
applyMigrationFailure =
    withBootstrappedTempDb $ \conn -> do
      m1 <- newMigration "second"
      m2 <- newMigration "third"

      let m1' = m1 { mApply = "CREATE TABLE valid (a int)" }
      let m2' = m2 { mApply = "INVALID SQL" }

      -- Apply the migrations, ignore exceptions
      ignoreSqlExceptions $ withTransaction conn $ \conn' -> do
        applyMigration conn' m1'
        applyMigration conn' m2'

      -- Check that none of the migrations were installed
      installed <- getMigrations conn
      return $ "successfully roll back failed apply" ~: ["root"] ~=? installed

revertMigrationFailure :: IO Test
revertMigrationFailure =
    withBootstrappedTempDb $ \conn -> do
      m1 <- newMigration "second"
      m2 <- newMigration "third"

      let m1' = m1 { mApply = "CREATE TABLE valid (a int)"
                   , mRevert = Just "DROP TABLE valid"}
      let m2' = m2 { mApply = "SELECT * FROM valid"
                   , mRevert = Just "INVALID SQL"}

      withTransaction conn $ \conn' -> do
        applyMigration conn' m1'
        applyMigration conn' m2'

      installedBeforeRevert <- getMigrations conn

      -- Revert the migrations, ignore exceptions
      ignoreSqlExceptions $ withTransaction conn $ \conn' -> do
        revertMigration conn' m2'
        revertMigration conn' m1'

      -- Check that none of the migrations were installed
      installed <- getMigrations conn
      return $ "successfully roll back failed revert" ~: installedBeforeRevert ~=? installed

revertMigrationNothing :: IO Test
revertMigrationNothing =
    withBootstrappedTempDb $ \conn -> do
      m1 <- newMigration "second"

      let m1' = m1 { mApply = "SELECT 1"
                   , mRevert = Nothing }

      withTransaction conn $ \conn' -> do
        applyMigration conn' m1'

      installedAfterApply <- getMigrations conn
      assertEqual "Check that the migration was applied" installedAfterApply ["root", "second"]

      -- Revert the migration, which should do nothing EXCEPT remove
      -- it from the installed list
      withTransaction conn $ \conn' -> do
        revertMigration conn' m1'

      installed <- getMigrations conn
      return $ test $ assertEqual "Check that the migration was reverted" installed ["root"]

revertMigrationJust :: IO Test
revertMigrationJust =
    withBootstrappedTempDb $ \conn -> do
      m1 <- newMigration "second"

      let m1' = m1 { mApply = "CREATE TABLE temp (a int)"
                   , mRevert = Just "DROP TABLE temp" }

      withTransaction conn $ \conn' -> do
        applyMigration conn' m1'

      installedAfterApply <- getMigrations conn
      assertEqual "Check that the migration was applied" installedAfterApply ["root", "second"]

      -- Revert the migration, which should do nothing EXCEPT remove
      -- it from the installed list
      withTransaction conn $ \conn' -> do
        revertMigration conn' m1'

      installed <- getMigrations conn
      return $ test $ assertEqual "Check that the migration was reverted" installed ["root"]
