module LinearMigrationsTest (tests) where

import           InMemoryStore
import           Test.HUnit

import           Control.Monad.Reader                 (runReaderT)
import           Data.Either                          (isRight)
import           Data.Maybe                           (isJust, isNothing)
import           Database.Schema.Migrations.Migration
import           Database.Schema.Migrations.Store
import           Moo.CommandHandlers
import           Moo.Core

tests :: IO [Test]
tests = sequence [ addsMigration
                 , setsTimestamp
                 , selectsLatestMigrationAsDep
                 , selectsOnlyOneMigrationAsDep
                 , doesNotAddTimestampWhenLinearMigrationsAreDisabled
                 , doesNotAddDependencyWhenLinearMigrationsAreDisabled
                 ]

satisfies :: String -> a -> (a -> Bool) -> IO Test
satisfies m v f = return $ TestCase $ assertBool m (f v)

addsMigration :: IO Test
addsMigration = do
    state <- prepareState "first"
    mig <- addTestMigration state
    satisfies "Migration not added" mig isRight

setsTimestamp :: IO Test
setsTimestamp = do
    state <- prepareState "first"
    Right mig <- addTestMigration state
    satisfies "Timestamp not set" (mTimestamp mig) isJust

selectsLatestMigrationAsDep :: IO Test
selectsLatestMigrationAsDep = do
    state1 <- prepareState "first"
    _ <- addTestMigration state1
    state2 <- prepareStateWith state1 "second"
    Right mig <- addTestMigration state2
    return $ ["first"] ~=? mDeps mig

selectsOnlyOneMigrationAsDep :: IO Test
selectsOnlyOneMigrationAsDep = do
    state1 <- prepareState "first"
    _ <- addTestMigration state1
    state2 <- prepareStateWith state1 "second"
    _ <- addTestMigration state2
    state3 <- prepareStateWith state2 "third"
    Right mig <- addTestMigration state3
    return $ ["second"] ~=? mDeps mig

doesNotAddTimestampWhenLinearMigrationsAreDisabled :: IO Test
doesNotAddTimestampWhenLinearMigrationsAreDisabled = do
    state' <- prepareState "first"
    let state = state' { _appLinearMigrations = False }
    Right mig <- addTestMigration state
    satisfies "Timestamp should be Nothing" (mTimestamp mig) isNothing

doesNotAddDependencyWhenLinearMigrationsAreDisabled :: IO Test
doesNotAddDependencyWhenLinearMigrationsAreDisabled = do
    state1' <- prepareState "first"
    let state1 = state1' { _appLinearMigrations = False }
    _ <- addTestMigration state1
    state2 <- prepareStateWith state1 "second"
    Right mig <- addTestMigration state2
    satisfies "Dependencies should be empty" (mDeps mig) null

addTestMigration :: AppState -> IO (Either String Migration)
addTestMigration state = do
    let store = _appStore state
    let [migrationId] = _appRequiredArgs state
    runReaderT (newCommand $ _appStoreData state) state
    loadMigration store migrationId

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
