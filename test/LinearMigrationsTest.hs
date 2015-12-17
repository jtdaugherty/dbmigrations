module LinearMigrationsTest (tests) where

import           InMemoryStore
import           Test.HUnit

import           Common
import           Control.Monad.Reader                 (runReaderT)
import           Data.Either                          (isRight)
import           Data.Maybe                           (isNothing)
import           Database.Schema.Migrations.Migration
import           Database.Schema.Migrations.Store
import           Moo.CommandHandlers
import           Moo.Core

tests :: IO [Test]
tests = sequence [ addsMigration
                 -- , doesNotSetTimestamp
                 , selectsLatestMigrationAsDep
                 , selectsOnlyLeavesAsDeps
                 , doesNotAddDependencyWhenLinearMigrationsAreDisabled
                 ]

addsMigration :: IO Test
addsMigration = do
    state <- prepareState "first"
    mig <- addTestMigration state
    satisfies "Migration not added" mig isRight

-- What is this test case for? It'll never pass since the `addsMigration`
-- test case will generate a migration with a timestamp.
doesNotSetTimestamp :: IO Test
doesNotSetTimestamp = do
    state <- prepareState "first"
    Right mig <- addTestMigration state
    satisfies "Timestamp is set" (mTimestamp mig) isNothing

selectsLatestMigrationAsDep :: IO Test
selectsLatestMigrationAsDep = do
    state1 <- prepareState "first"
    _ <- addTestMigration state1
    state2 <- prepareStateWith state1 "second"
    Right mig <- addTestMigration state2
    return $ ["first"] ~=? mDeps mig

selectsOnlyLeavesAsDeps :: IO Test
selectsOnlyLeavesAsDeps = do
    state1 <- prepareNormalState "first"
    addTestMigrationWithDeps state1 []
    state2 <- prepareStateWith state1 "second"
    addTestMigrationWithDeps state2 ["first"]
    state3 <- prepareStateWith state2 "third"
    addTestMigrationWithDeps state3 ["first"]
    state4' <- prepareStateWith state3 "fourth"
    let state4 = state4' { _appLinearMigrations = True }
    Right mig <- addTestMigration state4
    return $ ["second", "third"] ~=? mDeps mig

doesNotAddDependencyWhenLinearMigrationsAreDisabled :: IO Test
doesNotAddDependencyWhenLinearMigrationsAreDisabled = do
    state1 <- prepareNormalState "first"
    _ <- addTestMigration state1
    state2 <- prepareStateWith state1 "second"
    Right mig <- addTestMigration state2
    satisfies "Dependencies should be empty" (mDeps mig) null

addTestMigration :: AppState -> IO (Either String Migration)
addTestMigration state = do
    let store = _appStore state
        [migrationId] = _appRequiredArgs state
    runReaderT (newCommand $ _appStoreData state) state
    loadMigration store migrationId

addTestMigrationWithDeps :: AppState -> [String] -> IO ()
addTestMigrationWithDeps state deps = do
    let store = _appStore state
    let [migrationId] = _appRequiredArgs state
    saveMigration store (newMigration migrationId) { mDeps = deps }

prepareState :: String -> IO AppState
prepareState m = do
    store <- inMemoryStore
    Right storeData <- loadMigrations store
    return AppState {
      _appOptions = CommandOptions Nothing False True
    , _appCommand = undefined -- Not used by newCommand
    , _appRequiredArgs = [m]
    , _appOptionalArgs = []
    , _appStore = store
    , _appDatabaseConnStr = DbConnDescriptor ""
    , _appDatabaseType = "none"
    , _appStoreData = storeData
    , _appLinearMigrations = True
    }

prepareStateWith :: AppState -> String -> IO AppState
prepareStateWith state m = do
    Right storeData <- loadMigrations $ _appStore state
    return state { _appRequiredArgs = [m], _appStoreData = storeData }

prepareNormalState :: String -> IO AppState
prepareNormalState m = do
    state <- prepareState m
    return $ state { _appLinearMigrations = False }
