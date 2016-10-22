{-# LANGUAGE OverloadedStrings #-}

-- | A test that is not executed as part of this package's test suite but rather
-- acts as a conformance test suit for database specific backend
-- implementations. All backend specific executable packages are expected to
-- have a test suite that runs this test.
module Database.Schema.Migrations.Test.BackendTest
    ( BackendConnection (..)
    , tests
    ) where

import Control.Monad ( forM_ )
import Test.HUnit

import Database.Schema.Migrations.Migration ( Migration(..), newMigration )
import Database.Schema.Migrations.Backend ( Backend(..) )

-- | A typeclass for database connections that needs to implemented for each
-- specific database type to use this test.
class BackendConnection c where

    -- | Whether this backend supports transactional DDL; if it doesn't,
    -- we'll skip any tests that rely on that behavior.
    supportsTransactionalDDL :: c -> Bool

    -- | Commits the current transaction.
    commit :: c -> IO ()

    -- | Executes an IO action inside a transaction.
    withTransaction :: c -> (c -> IO a) -> IO a

    -- | Retrieves a list of all tables in the current database/scheme.
    getTables :: c -> IO [String]

    catchAll :: c -> (IO a -> IO a -> IO a)

    -- | Returns a backend instance.
    makeBackend :: c -> Backend

-- | The full test suite a proper dbmigrations backend needs to comply to.
fullTestSuite :: BackendConnection bc => [bc -> IO ()]
fullTestSuite =
    [ isBootstrappedFalseTest
    , bootstrapTest
    , isBootstrappedTrueTest
    , applyMigrationFailure
    , applyMigrationSuccess
    , revertMigrationFailure
    , revertMigrationNothing
    , revertMigrationJust
    ]

-- | Some test (applyMigrationFailure in particular) assume that DDL statements
-- support transactions. MySQL does not support DDL transactions so we exclude
-- this test for MySQL backends.
nonTransactionalDdlTestSuite :: BackendConnection bc => [bc -> IO ()]
nonTransactionalDdlTestSuite =
    [ isBootstrappedFalseTest
    , bootstrapTest
    , isBootstrappedTrueTest
    , applyMigrationSuccess
    , revertMigrationFailure
    , revertMigrationNothing
    , revertMigrationJust
    ]

tests :: BackendConnection bc => bc -> IO ()
tests conn = do
  let acts = case supportsTransactionalDDL conn of
             False -> nonTransactionalDdlTestSuite
             True  -> fullTestSuite
  forM_ acts $ \act -> do
               commit conn
               act conn

bootstrapTest :: BackendConnection bc => bc -> IO ()
bootstrapTest conn = do
  let backend = makeBackend conn
  bs <- getBootstrapMigration backend
  applyMigration backend bs
  assertEqual "installed_migrations table exists" ["installed_migrations"] =<< getTables conn
  assertEqual "successfully bootstrapped" [mId bs] =<< getMigrations backend

isBootstrappedTrueTest :: BackendConnection bc => bc -> IO ()
isBootstrappedTrueTest conn = do
  result <- isBootstrapped $ makeBackend conn
  assertBool "Bootstrapped check" result

isBootstrappedFalseTest :: BackendConnection bc => bc -> IO ()
isBootstrappedFalseTest conn = do
  result <- isBootstrapped $ makeBackend conn
  assertBool "Bootstrapped check" $ not result

ignoreSqlExceptions :: BackendConnection bc => bc -> IO a -> IO (Maybe a)
ignoreSqlExceptions conn act =
  (catchAll conn)
    (act >>= return . Just)
    (return Nothing)

applyMigrationSuccess :: BackendConnection bc => bc -> IO ()
applyMigrationSuccess conn = do
    let backend = makeBackend conn

    let m1 = (newMigration "validMigration") { mApply = "CREATE TABLE valid1 (a int); CREATE TABLE valid2 (a int);" }

    -- Apply the migrations, ignore exceptions
    withTransaction conn $ \conn' -> applyMigration (makeBackend conn') m1

    -- Check that none of the migrations were installed
    assertEqual "Installed migrations" ["root", "validMigration"] =<< getMigrations backend
    assertEqual "Installed tables" ["installed_migrations", "valid1", "valid2"] =<< getTables conn

-- |Does a failure to apply a migration imply a transaction rollback?
applyMigrationFailure :: BackendConnection bc => bc -> IO ()
applyMigrationFailure conn = do
    let backend = makeBackend conn

    let m1 = (newMigration "second") { mApply = "CREATE TABLE validButTemporary (a int)" }
        m2 = (newMigration "third") { mApply = "INVALID SQL" }

    -- Apply the migrations, ignore exceptions
    ignoreSqlExceptions conn $ withTransaction conn $ \conn' -> do
        let backend' = makeBackend conn'
        applyMigration backend' m1
        applyMigration backend' m2

    -- Check that none of the migrations were installed
    assertEqual "Installed migrations" ["root"] =<< getMigrations backend
    assertEqual "Installed tables" ["installed_migrations"] =<< getTables conn

revertMigrationFailure :: BackendConnection bc => bc -> IO ()
revertMigrationFailure conn = do
    let backend = makeBackend conn

    let m1 = (newMigration "second") { mApply = "CREATE TABLE validRMF (a int)"
                                     , mRevert = Just "DROP TABLE validRMF"}
        m2 = (newMigration "third") { mApply = "SELECT * FROM validRMF"
                                    , mRevert = Just "INVALID REVERT SQL"}

    applyMigration backend m1
    applyMigration backend m2

    installedBeforeRevert <- getMigrations backend

    commitBackend backend

    -- Revert the migrations, ignore exceptions; the revert will fail,
    -- but withTransaction will roll back.
    ignoreSqlExceptions conn $ withTransaction conn $ \conn' -> do
        let backend' = makeBackend conn'
        revertMigration backend' m2
        revertMigration backend' m1

    -- Check that none of the migrations were reverted
    assertEqual "successfully roll back failed revert" installedBeforeRevert
        =<< getMigrations backend

revertMigrationNothing :: BackendConnection bc => bc -> IO ()
revertMigrationNothing conn = do
    let backend = makeBackend conn

    let m1 = (newMigration "second") { mApply = "SELECT 1"
                                     , mRevert = Nothing }

    applyMigration backend m1

    installedAfterApply <- getMigrations backend
    assertBool "Check that the migration was applied" $ "second" `elem` installedAfterApply

    -- Revert the migration, which should do nothing EXCEPT remove it
    -- from the installed list
    revertMigration backend m1

    installed <- getMigrations backend
    assertBool "Check that the migration was reverted" $ not $ "second" `elem` installed

revertMigrationJust :: BackendConnection bc => bc -> IO ()
revertMigrationJust conn = do
    let name = "revertable"
        backend = makeBackend conn

    let m1 = (newMigration name) { mApply = "CREATE TABLE the_test_table (a int)"
                                 , mRevert = Just "DROP TABLE the_test_table" }

    applyMigration backend m1

    installedAfterApply <- getMigrations backend
    assertBool "Check that the migration was applied" $ name `elem` installedAfterApply

    -- Revert the migration, which should do nothing EXCEPT remove it
    -- from the installed list
    revertMigration backend m1

    installed <- getMigrations backend
    assertBool "Check that the migration was reverted" $ not $ name `elem` installed
