module Database.Schema.Migrations
    ( missingMigrations
    , createNewMigration
    )
where

import qualified Data.Set as Set
import qualified Data.Map as Map

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
