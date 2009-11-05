module BackendTest where

import Test.HUnit
import Control.Monad ( (=<<), forM_ )

import Database.Schema.Migrations.Backend.HDBC ()
import Database.Schema.Migrations.Migration ( Migration(..), newMigration )
import Database.Schema.Migrations.Backend ( Backend(..) )

import Database.HDBC ( IConnection(..), catchSql, withTransaction )

tests :: (IConnection a) => a -> IO ()
tests conn = do
  let acts = [ isBootstrappedFalseTest
             , bootstrapTest
             , isBootstrappedTrueTest
             , applyMigrationFailure
             , applyMigrationSuccess
             , revertMigrationFailure
             , revertMigrationNothing
             , revertMigrationJust
             ]
  forM_ acts $ \act -> do
               commit conn
               act conn

bootstrapTest :: (IConnection a) => a -> IO ()
bootstrapTest conn = do
  bs <- getBootstrapMigration conn
  applyMigration conn bs
  assertEqual "installed_migrations table exists" ["installed_migrations"] =<< getTables conn
  assertEqual "successfully bootstrapped" [mId bs] =<< getMigrations conn

isBootstrappedTrueTest :: (IConnection a) => a -> IO ()
isBootstrappedTrueTest conn = do
  result <- isBootstrapped conn
  assertBool "Bootstrapped check" result

isBootstrappedFalseTest :: (IConnection a) => a -> IO ()
isBootstrappedFalseTest conn = do
  result <- isBootstrapped conn
  assertBool "Bootstrapped check" $ not result

ignoreSqlExceptions :: IO a -> IO (Maybe a)
ignoreSqlExceptions act = (act >>= return . Just) `catchSql`
                       (\_ -> return Nothing)

applyMigrationSuccess :: (IConnection a) => a -> IO ()
applyMigrationSuccess conn = do
    m1 <- newMigration "validMigration"

    let m1' = m1 { mApply = "CREATE TABLE valid1 (a int); CREATE TABLE valid2 (a int);" }

    -- Apply the migrations, ignore exceptions
    withTransaction conn $ \conn' -> applyMigration conn' m1'

    -- Check that none of the migrations were installed
    assertEqual "Installed migrations" ["root", "validMigration"] =<< getMigrations conn
    assertEqual "Installed tables" ["installed_migrations", "valid1", "valid2"] =<< getTables conn

-- |Does a failure to apply a migration imply a transaction rollback?
applyMigrationFailure :: (IConnection a) => a -> IO ()
applyMigrationFailure conn = do
    m1 <- newMigration "second"
    m2 <- newMigration "third"

    let m1' = m1 { mApply = "CREATE TABLE validButTemporary (a int)" }
    let m2' = m2 { mApply = "INVALID SQL" }

    -- Apply the migrations, ignore exceptions
    ignoreSqlExceptions $ withTransaction conn $ \conn' -> do
                               applyMigration conn' m1'
                               applyMigration conn' m2'

    -- Check that none of the migrations were installed
    assertEqual "Installed migrations" ["root"] =<< getMigrations conn
    assertEqual "Installed tables" ["installed_migrations"] =<< getTables conn

revertMigrationFailure :: (IConnection a) => a -> IO ()
revertMigrationFailure conn = do
    m1 <- newMigration "second"
    m2 <- newMigration "third"

    let m1' = m1 { mApply = "CREATE TABLE validRMF (a int)"
                 , mRevert = Just "DROP TABLE validRMF"}
    let m2' = m2 { mApply = "SELECT * FROM validRMF"
                 , mRevert = Just "INVALID REVERT SQL"}

    applyMigration conn m1'
    applyMigration conn m2'

    installedBeforeRevert <- getMigrations conn

    commit conn

    -- Revert the migrations, ignore exceptions; the revert will fail,
    -- but withTransaction will roll back.
    ignoreSqlExceptions $ withTransaction conn $ \conn' -> do
      revertMigration conn' m2'
      revertMigration conn' m1'

    -- Check that none of the migrations were reverted
    assertEqual "successfully roll back failed revert" installedBeforeRevert
        =<< getMigrations conn

revertMigrationNothing :: (IConnection a) => a -> IO ()
revertMigrationNothing conn = do
    m1 <- newMigration "second"

    let m1' = m1 { mApply = "SELECT 1"
                 , mRevert = Nothing }

    applyMigration conn m1'

    installedAfterApply <- getMigrations conn
    assertBool "Check that the migration was applied" $ "second" `elem` installedAfterApply

    -- Revert the migration, which should do nothing EXCEPT remove it
    -- from the installed list
    revertMigration conn m1'

    installed <- getMigrations conn
    assertBool "Check that the migration was reverted" $ not $ "second" `elem` installed

revertMigrationJust :: (IConnection a) => a -> IO ()
revertMigrationJust conn = do
    let name = "revertable"
    m1 <- newMigration name

    let m1' = m1 { mApply = "CREATE TABLE the_test_table (a int)"
                 , mRevert = Just "DROP TABLE the_test_table" }

    applyMigration conn m1'

    installedAfterApply <- getMigrations conn
    assertBool "Check that the migration was applied" $ name `elem` installedAfterApply

    -- Revert the migration, which should do nothing EXCEPT remove it
    -- from the installed list
    revertMigration conn m1'

    installed <- getMigrations conn
    assertBool "Check that the migration was reverted" $ not $ name `elem` installed
