{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Schema.Migrations.Backend
    ( Backend(..)
    )
where

import Database.Schema.Migrations.Migration
    ( Migration(..) )

class (Monad m) => Backend b m where
    -- |The migration necessary to bootstrap a database with this
    -- connection interface.  This might differ slightly from one
    -- backend to another.
    getBootstrapMigration :: b -> m Migration
    isBootstrapped :: b -> m Bool
    applyMigration :: b -> Migration -> m ()
    revertMigration :: b -> Migration -> m ()
    getMigrations :: b -> m [String]
