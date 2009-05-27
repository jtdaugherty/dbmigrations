{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Schema.Migrations.Store
    ( MigrationStore(..)
    , loadMigrations
    , depGraphFromStore
    )
where

import Data.Maybe ( catMaybes )
import qualified Data.Map as Map

import Database.Schema.Migrations.Migration
    ( Migration(..)
    , MigrationMap
    )
import Database.Schema.Migrations.Dependencies
    ( DependencyGraph(..)
    , mkDepGraph
    )

class (Monad m) => MigrationStore s m where
    -- Load a migration from the store
    loadMigration :: s -> String -> m (Maybe Migration)

    -- Save a migration to the store
    saveMigration :: s -> Migration -> m ()

    -- Return a list of all available migrations' names
    getMigrations :: s -> m [String]

    -- Return the full representation of a given migration name;
    -- mostly for filesystem stores, where the "full" representation
    -- includes the store path.
    fullMigrationName :: s -> String -> m String
    fullMigrationName _ name = return name

-- |Load migrations recursively from the specified path into the
-- MigrationMap state.
loadMigrations :: (MigrationStore s m) => s -> m MigrationMap
loadMigrations store = do
  migrations <- getMigrations store
  loaded <- mapM (\name -> loadMigration store name) migrations
  return $ Map.fromList $ [ (mId e, e) | e <- catMaybes $ loaded ]

depGraphFromStore :: (MigrationStore s m) => s -> m (Either String (DependencyGraph Migration))
depGraphFromStore store = do
  migrationIds <- getMigrations store
  migrations <- mapM (loadMigration store) migrationIds
  let loaded = catMaybes migrations
  return $ mkDepGraph loaded