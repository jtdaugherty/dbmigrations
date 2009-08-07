{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Schema.Migrations.Backend
    ( Backend(..)
    )
where

import Database.Schema.Migrations.Migration
    ( Migration(..) )

-- |A Backend represents a database engine backend such as MySQL or
-- SQLite.  A Backend supplies relatively low-level functions for
-- inspecting the backend's state, applying migrations, and reverting
-- migrations.  A Backend also supplies the migration necessary to
-- "bootstrap" a backend so that it can track which migrations are
-- installed.
class (Monad m) => Backend b m where
    -- |The migration necessary to bootstrap a database with this
    -- connection interface.  This might differ slightly from one
    -- backend to another.
    getBootstrapMigration :: b -> m Migration
    isBootstrapped :: b -> m Bool
    applyMigration :: b -> Migration -> m ()
    revertMigration :: b -> Migration -> m ()
    getMigrations :: b -> m [String]
