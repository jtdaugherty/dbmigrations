module ConfigurationTest (tests) where

import           Control.Exception  (SomeException, try)
import           Data.Either        (isLeft, isRight)
import           System.Directory
import           System.Environment (setEnv, unsetEnv)
import           Test.HUnit

import           Common
import           Moo.Core

tests :: IO [Test]
tests = sequence [prepareTestEnv >> e | e <- entries]
    where entries = [ loadsConfigFile
                    , loadsPropertiesFromFile
                    , loadsDefaultConfigFile
                    , environmentOverridesProperties
                    , ifNoConfigFileIsAvailableEnvironmentIsUsed
                    , throwsWhenConfigFileIsInvalid
                    , returnsErrorWhenNotAllPropertiesAreSet
                    ]

prepareTestEnv :: IO ()
prepareTestEnv = do
    setCurrentDirectory $ testFile "config_loading"
    unsetEnv "DBM_DATABASE_TYPE"
    unsetEnv "DBM_DATABASE"
    unsetEnv "DBM_MIGRATION_STORE"
    unsetEnv "DBM_LINEAR_MIGRATIONS"

loadsConfigFile :: IO Test
loadsConfigFile = do
    cfg' <- loadConfiguration (Just "cfg1.cfg")
    satisfies "File not loaded" cfg' isRight

loadsPropertiesFromFile :: IO Test
loadsPropertiesFromFile = do
    Right cfg <- loadConfiguration (Just "cfg1.cfg")
    return
        (
        _connectionString   cfg ~?= "connection" .&&.
        _databaseType       cfg ~?= "dbtype"     .&&.
        _migrationStorePath cfg ~?= "store"      .&&.
        _linearMigrations   cfg ~?= True
        )

loadsDefaultConfigFile :: IO Test
loadsDefaultConfigFile = do
    Right cfg <- loadConfiguration Nothing
    return
        (
        _connectionString   cfg ~?= "mooconn"  .&&.
        _databaseType       cfg ~?= "moodb"    .&&.
        _migrationStorePath cfg ~?= "moostore" .&&.
        _linearMigrations   cfg ~?= True
        )

environmentOverridesProperties :: IO Test
environmentOverridesProperties = do
    setEnv "DBM_DATABASE_TYPE" "envdb"
    setEnv "DBM_DATABASE" "envconn"
    setEnv "DBM_MIGRATION_STORE" "envstore"
    setEnv "DBM_LINEAR_MIGRATIONS" "off"
    Right cfg <- loadConfiguration (Just "cfg1.cfg")
    return
        (
        _connectionString   cfg ~?= "envconn"  .&&.
        _databaseType       cfg ~?= "envdb"    .&&.
        _migrationStorePath cfg ~?= "envstore" .&&.
        _linearMigrations   cfg ~?= False
        )

ifNoConfigFileIsAvailableEnvironmentIsUsed :: IO Test
ifNoConfigFileIsAvailableEnvironmentIsUsed = do
    setCurrentDirectory $ testFile ""
    setEnv "DBM_DATABASE_TYPE" "envdb"
    setEnv "DBM_DATABASE" "envconn"
    setEnv "DBM_MIGRATION_STORE" "envstore"
    setEnv "DBM_LINEAR_MIGRATIONS" "off"
    Right cfg <- loadConfiguration Nothing
    return
        (
        _connectionString   cfg ~?= "envconn"  .&&.
        _databaseType       cfg ~?= "envdb"    .&&.
        _migrationStorePath cfg ~?= "envstore" .&&.
        _linearMigrations   cfg ~?= False
        )

returnsErrorWhenNotAllPropertiesAreSet :: IO Test
returnsErrorWhenNotAllPropertiesAreSet = do
    cfg <- loadConfiguration (Just "missing.cfg")
    satisfies "Should return error" cfg isLeft

throwsWhenConfigFileIsInvalid :: IO Test
throwsWhenConfigFileIsInvalid = do
    c <- try $ loadConfiguration (Just "invalid.cfg")
    satisfies "Should throw" c (isLeft :: Either SomeException a -> Bool)

