module Database.Schema.Migrations.Backend
    ( Backend(..)
    )
where

import Database.HDBC ( IConnection )
import Database.Schema.Migrations.Migration
    ( Migration(..)
    , MigrationID
    , newMigration
    )

class (IConnection a) => Backend a where
    -- |The migration necessary to bootstrap a database with this
    -- connection interface.  This might differ slightly from one
    -- backend to another.
    getBootstrapMigration :: a -> IO Migration
    getBootstrapMigration _ = do
      m <- newMigration
      return $ m { mDesc = Just "An initial migration to bootstrap the database"
                 , mApply = ""
                 , mRevert = ""
                 }

    applyMigration :: a -> Migration -> IO ()
    revertMigration :: a -> Migration -> IO ()
    getMigrations :: a -> IO [MigrationID]