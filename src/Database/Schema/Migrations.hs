module Database.Schema.Migrations
    ( createNewMigration
    , ensureBootstrappedBackend
    , migrationsToApply
    )
where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe ( catMaybes )

import Database.Schema.Migrations.Dependencies ( dependencies )
import qualified Database.Schema.Migrations.Backend as B
import qualified Database.Schema.Migrations.Store as S
import Database.Schema.Migrations.Migration
    ( MigrationMap
    , Migration(..)
    , newMigration
    , MonadMigration
    )

missingMigrations :: (B.Backend b m) => b -> MigrationMap -> m [String]
missingMigrations backend storeMigrationMap = do
  let storeMigrations = Map.keys storeMigrationMap
  backendMigrations <- B.getMigrations backend

  return $ Set.toList $ Set.difference
         (Set.fromList storeMigrations)
         (Set.fromList backendMigrations)

createNewMigration :: (MonadMigration m, S.MigrationStore s m) =>
                      s -> String -> m (Either String ())
createNewMigration store name = do
  available <- S.getMigrations store
  fullPath <- S.fullMigrationName store name
  case name `elem` available of
    True -> return $ Left $ "Migration " ++ (show fullPath) ++ " already exists"
    False -> do
      new <- newMigration name
      let newWithDefaults = new { mDesc = Just "(Description here.)"
                                , mApply = "(Apply SQL here.)"
                                , mRevert = Just "(Revert SQL here.)"
                                }
      S.saveMigration store newWithDefaults
      return $ Right ()

ensureBootstrappedBackend :: (B.Backend b m) => b -> m ()
ensureBootstrappedBackend backend = do
  bsStatus <- B.isBootstrapped backend
  case bsStatus of
    True -> return ()
    False -> B.getBootstrapMigration backend >>= B.applyMigration backend

-- |Given a migration mapping computed from a MigrationStore, a
-- backend, and a migration to apply, return a processing error or a
-- list of migrations to apply, in order.
migrationsToApply :: (B.Backend b m) => MigrationMap -> b
                  -> Migration -> m (Either String [Migration])
migrationsToApply mapping backend migration = do
  let graph' = S.depGraphFromMapping mapping
  case graph' of
    Left e -> return $ Left e
    Right g -> run g

  where
    run graph = do
      allMissing <- missingMigrations backend mapping

      let deps = (dependencies graph $ mId migration) ++ [mId migration]
          namesToInstall = [ e | e <- deps, e `elem` allMissing ]
          loadedMigrations = catMaybes $ map (\k -> Map.lookup k mapping) namesToInstall

      return $ Right loadedMigrations
