dbmigrations changelog
----------------------

0.9.1
-----

- Restored default timestamp and description values in migrations
created by new migration command

0.9
---

- Fix 'moo' usage output to use correct program name
- Replaced Backend type class in favor of concrete Backend record type
- Added hdbcBackend constructor
- Backends now always run in IO rather than some MonadIO
- Removed monad parameter from MigrationStore (always use IO)
- Replaced MigrationStore type class with concrete MigrationStore type
- Added filesystem migration store constructor
- Improve configuration type so that it has been implicitly validated
- Made newMigration pure, made migration timestamps optional
- createNewMigration now takes a Migration for greater caller control
