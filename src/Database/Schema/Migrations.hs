module Database.Schema.Migrations
    ()
where

import Database.Schema.Migrations.Migration
import Database.Schema.Migrations.Store
import Database.Schema.Migrations.Backend
import Database.Schema.Migrations.Dependencies

migrate :: (Backend b, MigrationStore s) => b -> s -> Migration -> IO ()
migrate = undefined

revert :: (Backend b, MigrationStore s) => b -> s -> Migration -> IO ()
revert = undefined
         -- given a migration, recursively revert all migrations in
         -- the backend that depend on m, then revert m