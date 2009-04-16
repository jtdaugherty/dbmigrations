module Database.Schema.Migrations.Store
    ( MigrationStore
    )
where

import Database.Schema.Migrations.Migration ( Migration )

class MigrationStore a where
    createMigration :: a -> Migration -> IO ()
    saveMigration :: a -> Migration -> IO ()
    loadMigration :: a -> Int -> IO (Maybe Migration)
    getMigrations :: a -> IO [Migration]