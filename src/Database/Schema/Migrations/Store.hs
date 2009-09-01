{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Schema.Migrations.Store
    ( MigrationStore(..)
    , MapValidationError(..)
    , loadMigrations
    , depGraphFromMapping
    , validateMigrationMap
    )
where

import Data.Maybe ( catMaybes, isJust )
import Control.Monad ( mzero )
import qualified Data.Map as Map

import Database.Schema.Migrations.Migration
    ( Migration(..)
    , MigrationMap
    )
import Database.Schema.Migrations.Dependencies
    ( DependencyGraph(..)
    , mkDepGraph
    , depsOf
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

data MapValidationError = DependencyReferenceError String String

instance Show MapValidationError where
    show (DependencyReferenceError from to) =
        "Migration " ++ (show from) ++ " references nonexistent dependency " ++ show to

-- |Load migrations recursively from the specified path into the
-- MigrationMap state.
loadMigrations :: (MigrationStore s m) => s -> m (Either [MapValidationError] MigrationMap)
loadMigrations store = do
  migrations <- getMigrations store
  loaded <- mapM (\name -> loadMigration store name) migrations
  let mMap = Map.fromList $ [ (mId e, e) | e <- catMaybes $ loaded ]
      validationErrors = validateMigrationMap mMap
  return $ if null validationErrors then Right mMap else Left validationErrors

validateMigrationMap :: MigrationMap -> [MapValidationError]
validateMigrationMap mMap = do
  (_, m) <- Map.toList mMap
  validateSingleMigration mMap m

validateSingleMigration :: MigrationMap -> Migration -> [MapValidationError]
validateSingleMigration mMap m = do
  depId <- depsOf m
  if isJust $ Map.lookup depId mMap then
      mzero else
      return $ DependencyReferenceError (mId m) depId

depGraphFromMapping :: MigrationMap -> Either String (DependencyGraph Migration)
depGraphFromMapping mapping = mkDepGraph $ Map.elems mapping