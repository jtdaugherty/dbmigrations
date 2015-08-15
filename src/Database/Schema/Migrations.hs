-- |This module provides a high-level interface for the rest of this
-- library.
module Database.Schema.Migrations
    ( createNewMigration
    , ensureBootstrappedBackend
    , migrationsToApply
    , migrationsToRevert
    , missingMigrations
    )
where

import qualified Data.Set as Set
import Data.Maybe ( catMaybes )

import Database.Schema.Migrations.Dependencies
    ( dependencies
    , reverseDependencies
    )
import qualified Database.Schema.Migrations.Backend as B
import qualified Database.Schema.Migrations.Store as S
import Database.Schema.Migrations.Migration
    ( Migration(..)
    , newMigration
    )

-- |Given a 'B.Backend' and a 'S.MigrationMap', query the backend and
-- return a list of migration names which are available in the
-- 'S.MigrationMap' but which are not installed in the 'B.Backend'.
missingMigrations :: B.Backend -> S.StoreData -> IO [String]
missingMigrations backend storeData = do
  let storeMigrationNames = map mId $ S.storeMigrations storeData
  backendMigrations <- B.getMigrations backend

  return $ Set.toList $ Set.difference
         (Set.fromList storeMigrationNames)
         (Set.fromList backendMigrations)

-- |Create a new migration and store it in the 'S.MigrationStore'.
createNewMigration :: S.MigrationStore -- ^ The 'S.MigrationStore' in which to create a new migration
                   -> Migration -- ^ The new migration
                   -> IO (Either String Migration)
createNewMigration store newM = do
  available <- S.getMigrations store
  case mId newM `elem` available of
    True -> do
      fullPath <- S.fullMigrationName store (mId newM)
      return $ Left $ "Migration " ++ (show fullPath) ++ " already exists"
    False -> do
      S.saveMigration store newM
      return $ Right newM

-- |Given a 'B.Backend', ensure that the backend is ready for use by
-- bootstrapping it.  This entails installing the appropriate database
-- elements to track installed migrations.  If the backend is already
-- bootstrapped, this has no effect.
ensureBootstrappedBackend :: B.Backend -> IO ()
ensureBootstrappedBackend backend = do
  bsStatus <- B.isBootstrapped backend
  case bsStatus of
    True -> return ()
    False -> B.getBootstrapMigration backend >>= B.applyMigration backend

-- |Given a migration mapping computed from a MigrationStore, a
-- backend, and a migration to apply, return a list of migrations to
-- apply, in order.
migrationsToApply :: S.StoreData -> B.Backend
                  -> Migration -> IO [Migration]
migrationsToApply storeData backend migration = do
  let graph = S.storeDataGraph storeData

  allMissing <- missingMigrations backend storeData

  let deps = (dependencies graph $ mId migration) ++ [mId migration]
      namesToInstall = [ e | e <- deps, e `elem` allMissing ]
      loadedMigrations = catMaybes $ map (S.storeLookup storeData) namesToInstall

  return loadedMigrations

-- |Given a migration mapping computed from a MigrationStore, a
-- backend, and a migration to revert, return a list of migrations to
-- revert, in order.
migrationsToRevert :: S.StoreData -> B.Backend
                   -> Migration -> IO [Migration]
migrationsToRevert storeData backend migration = do
  let graph = S.storeDataGraph storeData

  allInstalled <- B.getMigrations backend

  let rDeps = (reverseDependencies graph $ mId migration) ++ [mId migration]
      namesToRevert = [ e | e <- rDeps, e `elem` allInstalled ]
      loadedMigrations = catMaybes $ map (S.storeLookup storeData) namesToRevert

  return loadedMigrations
