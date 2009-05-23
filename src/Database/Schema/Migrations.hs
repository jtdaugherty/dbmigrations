module Database.Schema.Migrations
    ( missingMigrations
    )
where

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Database.Schema.Migrations.Backend as B
import Database.Schema.Migrations.Migration ( MigrationMap )

missingMigrations :: (B.Backend b m) => b -> MigrationMap -> m [String]
missingMigrations backend storeMigrationMap = do
  let storeMigrations = Map.keys storeMigrationMap
  backendMigrations <- B.getMigrations backend

  return $ Set.toList $ Set.difference
         (Set.fromList storeMigrations)
         (Set.fromList backendMigrations)
