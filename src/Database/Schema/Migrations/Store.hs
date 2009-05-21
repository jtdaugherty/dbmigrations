module Database.Schema.Migrations.Store
    ( MigrationStore(..)
    )
where

import Database.Schema.Migrations.Migration
    ( Migration
    )

class MigrationStore a where
    getMigrations :: a -> IO [Migration]
