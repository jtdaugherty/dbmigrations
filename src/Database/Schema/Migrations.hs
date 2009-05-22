module Database.Schema.Migrations
    ( missingMigrations
    )
where

import qualified Data.Set as Set

import qualified Database.Schema.Migrations.Store as MS
import qualified Database.Schema.Migrations.Backend as B
import Database.Schema.Migrations.Migration ( mId )

missingMigrations :: (B.Backend b m, MS.MigrationStore s m) => b -> s -> m [String]
missingMigrations backend store = do
  sm <- MS.getMigrations store
  let storeMigrations = map mId sm
  backendMigrations <- B.getMigrations backend

  return $ Set.toList $ Set.difference
         (Set.fromList storeMigrations)
         (Set.fromList backendMigrations)
