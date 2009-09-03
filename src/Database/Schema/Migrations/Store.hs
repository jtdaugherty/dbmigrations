{-# LANGUAGE MultiParamTypeClasses #-}
-- |This module provides an abstraction for a /migration store/, a
-- facility in which 'Migration's can be stored and from which they
-- can be loaded.  This module also provides functions for taking
-- 'Migration's from a store and converting them into the appropriate
-- intermediate types for use with the rest of this library.
module Database.Schema.Migrations.Store
    ( MigrationStore(..)
    , MapValidationError(..)
    , MigrationMap
    , loadMigrations
    , depGraphFromMapping
    , validateMigrationMap
    )
where

import Data.Maybe ( catMaybes, isJust )
import Control.Monad ( mzero )
import Control.Applicative ( (<$>) )
import qualified Data.Map as Map

import Database.Schema.Migrations.Migration
    ( Migration(..)
    )
import Database.Schema.Migrations.Dependencies
    ( DependencyGraph(..)
    , mkDepGraph
    , depsOf
    )

type MigrationMap = Map.Map String Migration

-- |A type class for types which represent a storage facility (and a
-- monad context in which to operate on the store).  A MigrationStore
-- is a facility in which new migrations can be created, and from
-- which existing migrations can be loaded.
class (Monad m) => MigrationStore s m where
    -- |Load a migration from the store.
    loadMigration :: s -> String -> m (Maybe Migration)

    -- |Save a migration to the store.
    saveMigration :: s -> Migration -> m ()

    -- |Return a list of all available migrations' names.
    getMigrations :: s -> m [String]

    -- |Return the full representation of a given migration name;
    -- mostly for filesystem stores, where the full representation
    -- includes the store path.
    fullMigrationName :: s -> String -> m String
    fullMigrationName _ name = return name

-- |A type for types of validation errors for migration maps.
data MapValidationError = DependencyReferenceError String String
                          -- ^ A migration claims a dependency on a
                          -- migration that does not exist.

instance Show MapValidationError where
    show (DependencyReferenceError from to) =
        "Migration " ++ (show from) ++ " references nonexistent dependency " ++ show to

-- |Load migrations from the specified 'MigrationStore', validate the
-- loaded migrations, and return errors or a 'MigrationMap' on
-- success.
loadMigrations :: (MigrationStore s m) => s -> m (Either [MapValidationError] MigrationMap)
loadMigrations store = do
  migrations <- getMigrations store
  loaded <- mapM (\name -> loadMigration store name) migrations
  let mMap = Map.fromList $ [ (mId e, e) | e <- catMaybes $ loaded ]
      validationErrors = validateMigrationMap mMap
  return $ if null validationErrors then Right mMap else Left validationErrors

-- |Validate a migration map.  Returns zero or more validation errors.
validateMigrationMap :: MigrationMap -> [MapValidationError]
validateMigrationMap mMap = do
  validateSingleMigration mMap =<< snd <$> Map.toList mMap

validateSingleMigration :: MigrationMap -> Migration -> [MapValidationError]
validateSingleMigration mMap m = do
  depId <- depsOf m
  if isJust $ Map.lookup depId mMap then
      mzero else
      return $ DependencyReferenceError (mId m) depId

-- |Create a 'DependencyGraph' from a 'MigrationMap'; returns Left if
-- the dependency graph cannot be constructed (e.g., due to a
-- dependency cycle) or Right on success.
depGraphFromMapping :: MigrationMap -> Either String (DependencyGraph Migration)
depGraphFromMapping mapping = mkDepGraph $ Map.elems mapping