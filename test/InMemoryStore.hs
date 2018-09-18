module InMemoryStore (inMemoryStore) where

import Data.Text ( Text )
import Data.String.Conversions ( cs )

import           Control.Concurrent.MVar
import           Database.Schema.Migrations.Migration
import           Database.Schema.Migrations.Store

type InMemoryData = [(Text, Migration)]

-- |Builds simple in-memory store that uses 'MVar' to preserve a list of
-- migrations.
inMemoryStore :: IO MigrationStore
inMemoryStore = do
    store <- newMVar []
    return MigrationStore {
      loadMigration = loadMigrationInMem store
    , saveMigration = saveMigrationInMem store
    , getMigrations = getMigrationsInMem store
    , fullMigrationName = return . cs
    }

loadMigrationInMem :: MVar InMemoryData -> Text -> IO (Either String Migration)
loadMigrationInMem store migId = withMVar store $ \migrations -> do
    let mig = lookup migId migrations
    return $ case mig of
        Just m -> Right m
        _      -> Left "Migration not found"

saveMigrationInMem :: MVar InMemoryData -> Migration -> IO ()
saveMigrationInMem store m = modifyMVar_ store $ return . ((mId m, m):)

getMigrationsInMem :: MVar InMemoryData -> IO [Text]
getMigrationsInMem store = withMVar store $ return . fmap fst
