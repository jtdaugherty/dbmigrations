module Database.Schema.Migrations.Store
    ( MigrationStore
    )
where

import qualified Data.Map as Map
import Database.Schema.Migrations.Migration
    ( Migration
    , MigrationID
    )

class MigrationStore a where
    createMigration :: a -> Migration -> IO ()
    saveMigration :: a -> Migration -> IO ()
    loadMigration :: a -> Int -> IO (Maybe Migration)
    getMigrations :: a -> IO [Migration]

data FilesystemStore = FSStore { storePath :: FilePath
                               , migrationMap :: Map.Map MigrationID Migration }

newFilesystemStore :: FilePath -> IO FilesystemStore
newFilesystemStore path = do
  let migrations = Map.empty
  return $ FSStore { storePath = path
                   , migrationMap = migrations }
