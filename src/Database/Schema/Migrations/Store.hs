{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Schema.Migrations.Store
    ( MigrationStore(..)
    , loadMigrations
    )
where

import Data.Maybe ( catMaybes )
import qualified Data.Map as Map

import Database.Schema.Migrations.Migration
    ( Migration(..)
    , MigrationMap
    )

class (Monad m) => MigrationStore s m where
    -- Load a migration from the store
    loadMigration :: s -> String -> m (Maybe Migration)

    -- Save a migration to the store
    saveMigration :: s -> Migration -> m ()

    -- Return a list of all available migrations' names
    getMigrations :: s -> m [String]

-- |Load migrations recursively from the specified path into the
-- MigrationMap state.
loadMigrations :: (MigrationStore s m) => s -> m MigrationMap
loadMigrations store = do
  migrations <- getMigrations store
  loaded <- mapM (\name -> loadMigration store name) migrations
  return $ Map.fromList $ [ (mId e, e) | e <- catMaybes $ loaded ]
