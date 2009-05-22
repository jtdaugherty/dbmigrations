{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Schema.Migrations.Store
    ( MigrationStore(..)
    )
where

import Database.Schema.Migrations.Migration
    ( Migration
    )

class (Monad m) => MigrationStore s m where
    getMigrations :: s -> m [Migration]
    saveMigration :: s -> Migration -> m ()