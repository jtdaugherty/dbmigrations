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
    , MonadMigration
    )

-- |Given a 'B.Backend' and a 'S.MigrationMap', query the backend and
-- return a list of migration names which are available in the
-- 'S.MigrationMap' but which are not installed in the 'B.Backend'.
missingMigrations :: (B.Backend b m) => b -> S.StoreData -> m [String]
missingMigrations backend storeData = do
  let storeMigrationNames = map mId $ S.storeMigrations storeData
  backendMigrations <- B.getMigrations backend

  return $ Set.toList $ Set.difference
         (Set.fromList storeMigrationNames)
         (Set.fromList backendMigrations)

-- |Create a new migration and store it in the 'S.MigrationStore',
-- with some of its fields initially set to defaults.
createNewMigration :: (MonadMigration m, S.MigrationStore s m)
                   => s -- ^ The 'S.MigrationStore' in which to create a new migration
                   -> String -- ^ The name of the new migration to create
                   -> [String] -- ^ The list of migration names on which the new migration should depend
                   -> m (Either String Migration)
createNewMigration store name deps = do
  available <- S.getMigrations store
  case name `elem` available of
    True -> do
      fullPath <- S.fullMigrationName store name
      return $ Left $ "Migration " ++ (show fullPath) ++ " already exists"
    False -> do
      new <- newMigration name
      let newWithDefaults = new { mDesc = Just "(Description here.)"
                                , mApply = "(Apply SQL here.)"
                                , mRevert = Just "(Revert SQL here.)"
                                , mDeps = deps
                                }
      S.saveMigration store newWithDefaults
      return $ Right newWithDefaults

-- |Given a 'B.Backend', ensure that the backend is ready for use by
-- bootstrapping it.  This entails installing the appropriate database
-- elements to track installed migrations.  If the backend is already
-- bootstrapped, this has no effect.
ensureBootstrappedBackend :: (B.Backend b m) => b -> m ()
ensureBootstrappedBackend backend = do
  bsStatus <- B.isBootstrapped backend
  case bsStatus of
    True -> return ()
    False -> B.getBootstrapMigration backend >>= B.applyMigration backend

-- |Given a migration mapping computed from a MigrationStore, a
-- backend, and a migration to apply, return a list of migrations to
-- apply, in order.
migrationsToApply :: (B.Backend b m) => S.StoreData -> b
                  -> Migration -> m [Migration]
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
migrationsToRevert :: (B.Backend b m) => S.StoreData -> b
                   -> Migration -> m [Migration]
migrationsToRevert storeData backend migration = do
  let graph = S.storeDataGraph storeData

  allInstalled <- B.getMigrations backend

  let rDeps = (reverseDependencies graph $ mId migration) ++ [mId migration]
      namesToRevert = [ e | e <- rDeps, e `elem` allInstalled ]
      loadedMigrations = catMaybes $ map (S.storeLookup storeData) namesToRevert

  return loadedMigrations
