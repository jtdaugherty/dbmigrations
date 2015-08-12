module Database.Schema.Migrations.Backend
    ( Backend(..)
    , rootMigrationName
    )
where

import Database.Schema.Migrations.Migration
    ( Migration(..) )

-- |Backend instances should use this as the name of the migration
-- returned by getBootstrapMigration; this migration is special
-- because it cannot be reverted.
rootMigrationName :: String
rootMigrationName = "root"

-- |A Backend represents a database engine backend such as MySQL or
-- SQLite.  A Backend supplies relatively low-level functions for
-- inspecting the backend's state, applying migrations, and reverting
-- migrations.  A Backend also supplies the migration necessary to
-- "bootstrap" a backend so that it can track which migrations are
-- installed.
data Backend =
    Backend { getBootstrapMigration :: IO Migration
            -- ^ The migration necessary to bootstrap a database with
            -- this connection interface. This might differ slightly
            -- from one backend to another.

            , isBootstrapped :: IO Bool
            -- ^ Returns whether the backend has been bootstrapped. A
            -- backend has been bootstrapped if is capable of tracking
            -- which migrations have been installed; the "bootstrap
            -- migration" provided by getBootstrapMigration should
            -- suffice to bootstrap the backend.

            , applyMigration :: Migration -> IO ()
            -- ^ Apply the specified migration on the backend.
            -- applyMigration does NOT assume control of the
            -- transaction, since it expects the transaction to
            -- (possibly) cover more than one applyMigration operation.
            -- The caller is expected to call commit at the appropriate
            -- time. If the application fails, the underlying SqlError
            -- is raised and a manual rollback may be necessary; for
            -- this, see withTransaction from HDBC.

            , revertMigration :: Migration -> IO ()
            -- ^ Revert the specified migration from the backend and
            -- record this action in the table which tracks installed
            -- migrations. revertMigration does NOT assume control of
            -- the transaction, since it expects the transaction to
            -- (possibly) cover more than one revertMigration operation.
            -- The caller is expected to call commit at the appropriate
            -- time. If the revert fails, the underlying SqlError is
            -- raised and a manual rollback may be necessary; for this,
            -- see withTransaction from HDBC. If the specified migration
            -- does not supply a revert instruction, this has no effect
            -- other than bookkeeping.

            , getMigrations :: IO [String]
            -- ^ Returns a list of installed migration names from the
            -- backend.

            , commitBackend :: IO ()
            -- ^ Commit changes to the backend.

            , rollbackBackend :: IO ()
            -- ^ Revert changes made to the backend since the current
            -- transaction began.

            , disconnectBackend :: IO ()
            -- ^ Disconnect from the backend.
            }
