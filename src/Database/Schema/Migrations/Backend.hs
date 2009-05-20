module Database.Schema.Migrations.Backend
    ( Backend(..)
    )
where

import Database.Schema.Migrations.Migration
    ( Migration(..) )

class Backend a where
    -- |The migration necessary to bootstrap a database with this
    -- connection interface.  This might differ slightly from one
    -- backend to another.
    getBootstrapMigration :: a -> IO Migration
    isBootstrapped :: a -> IO Bool
    applyMigration :: a -> Migration -> IO ()
    revertMigration :: a -> Migration -> IO ()
    getMigrations :: a -> IO [String]