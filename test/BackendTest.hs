{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BackendTest where

import Test.HUnit
import Control.Exception (Handler(..), catches)
import Control.Monad ( forM_ )

import Database.Schema.Migrations.Backend.HDBC (hdbcBackend)
import Database.Schema.Migrations.Backend.MySQL
import Database.Schema.Migrations.Migration ( Migration(..), newMigration )
import Database.Schema.Migrations.Backend ( Backend(..) )

import qualified Database.HDBC as HDBC
import qualified Database.MySQL.Simple as MySQL
import qualified Database.MySQL.Base as MySQL

data BackendConnection
  = forall a. HDBC.IConnection a => HDBCConnection a
  | MySQLConnection MySQL.Connection

migrationBackend :: BackendConnection -> Backend
migrationBackend (HDBCConnection c) = hdbcBackend c
migrationBackend (MySQLConnection c) = mysqlBackend c

commit :: BackendConnection -> IO ()
commit (HDBCConnection c) = HDBC.commit c
commit (MySQLConnection c) = MySQL.commit c

getTables :: BackendConnection -> IO [String]
getTables (HDBCConnection c) = HDBC.getTables c
getTables (MySQLConnection c) =
  fmap (map MySQL.fromOnly)
       (MySQL.query_ c "SHOW TABLES")

catchSql_ :: IO a -> IO a -> IO a
catchSql_ act handler =
  act `catches`
  [Handler (\(_ :: MySQL.FormatError) -> handler)
  ,Handler (\(_ :: MySQL.QueryError) -> handler)
  ,Handler (\(_ :: MySQL.MySQLError) -> handler)
  ,Handler (\(_ :: MySQL.ResultError) -> handler)
  ,Handler (\(_ :: HDBC.SqlError) -> handler)]

withTransaction
  :: BackendConnection -> (BackendConnection -> IO a) -> IO a
withTransaction (HDBCConnection c) transaction =
  HDBC.withTransaction c
                       (transaction . HDBCConnection)
withTransaction (MySQLConnection c) transaction =
  MySQL.withTransaction c
                        (transaction (MySQLConnection c))
tests :: BackendConnection -> IO ()
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

bootstrapTest :: BackendConnection -> IO ()
bootstrapTest conn = do
  let backend = migrationBackend conn
  bs <- getBootstrapMigration backend
  applyMigration backend bs
  assertEqual "installed_migrations table exists" ["installed_migrations"] =<< getTables conn
  assertEqual "successfully bootstrapped" [mId bs] =<< getMigrations backend

isBootstrappedTrueTest :: BackendConnection -> IO ()
isBootstrappedTrueTest conn = do
  result <- isBootstrapped $ migrationBackend conn
  assertBool "Bootstrapped check" result

isBootstrappedFalseTest :: BackendConnection -> IO ()
isBootstrappedFalseTest conn = do
  result <- isBootstrapped $ migrationBackend conn
  assertBool "Bootstrapped check" $ not result

ignoreSqlExceptions :: IO a -> IO (Maybe a)
ignoreSqlExceptions act = (act >>= return . Just) `catchSql_`
                          (return Nothing)

applyMigrationSuccess :: BackendConnection -> IO ()
applyMigrationSuccess conn = do
    let backend = migrationBackend conn

    let m1 = (newMigration "validMigration") { mApply = "CREATE TABLE valid1 (a int); CREATE TABLE valid2 (a int);" }

    -- Apply the migrations, ignore exceptions
    withTransaction conn $ \conn' -> applyMigration (migrationBackend conn') m1

    -- Check that none of the migrations were installed
    assertEqual "Installed migrations" ["root", "validMigration"] =<< getMigrations backend
    assertEqual "Installed tables" ["installed_migrations", "valid1", "valid2"] =<< getTables conn

-- |Does a failure to apply a migration imply a transaction rollback?
applyMigrationFailure :: BackendConnection -> IO ()
applyMigrationFailure conn = do
    let backend = migrationBackend conn

    let m1 = (newMigration "second") { mApply = "CREATE TABLE validButTemporary (a int)" }
        m2 = (newMigration "third") { mApply = "INVALID SQL" }

    -- Apply the migrations, ignore exceptions
    ignoreSqlExceptions $ withTransaction conn $ \conn' -> do
        let backend' = migrationBackend conn'
        applyMigration backend' m1
        applyMigration backend' m2

    -- Check that none of the migrations were installed
    assertEqual "Installed migrations" ["root"] =<< getMigrations backend
    assertEqual "Installed tables" ["installed_migrations"] =<< getTables conn

revertMigrationFailure :: BackendConnection -> IO ()
revertMigrationFailure conn = do
    let backend = migrationBackend conn

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
    ignoreSqlExceptions $ withTransaction conn $ \conn' -> do
        let backend' = migrationBackend conn'
        revertMigration backend' m2
        revertMigration backend' m1

    -- Check that none of the migrations were reverted
    assertEqual "successfully roll back failed revert" installedBeforeRevert
        =<< getMigrations backend

revertMigrationNothing :: BackendConnection -> IO ()
revertMigrationNothing conn = do
    let backend = migrationBackend conn

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

revertMigrationJust :: BackendConnection -> IO ()
revertMigrationJust conn = do
    let name = "revertable"
        backend = migrationBackend conn

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
