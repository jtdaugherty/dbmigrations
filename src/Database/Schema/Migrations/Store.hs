{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Schema.Migrations.Store
    ( MigrationStore(..)
    )
where

import Data.Maybe ( listToMaybe )

import Database.Schema.Migrations.Migration
    ( Migration(..)
    )

class (Monad m) => MigrationStore s m where
    getMigrations :: s -> m [Migration]

    getMigration :: s -> String -> m (Maybe Migration)
    getMigration s theId = do
      migrations <- getMigrations s
      return $ listToMaybe [ e | e <- migrations, mId e == theId ]

    saveMigration :: s -> Migration -> m ()