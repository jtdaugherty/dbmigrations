dbmigrations changelog
----------------------

1.0
---

- Added support for (optionally) adding timestamps to generated
migration filenames (thanks Matt Parsons <parsonsmatt@gmail.com>)
  * Adds flag for time stamp on file names
  * Adds configuration for timestamping filenames
- Added new "linear migrations" feature (thanks Jakub Fijałkowski
<fiolek94@gmail.com>, Andrew Martin <amartin@layer3com.com>). This
feature is an optional alternative to the default behavior: rather than
prompting the user for dependencies of new migrations (the default
behavior), linear mode automatically selects dependencies for new
migrations such that they depend on the smallest subset of migrations
necessary to (effectively) depend on all existing migrations, thus
"linearizing" the migration sequence. See MOO.TXT for details.
- Configuration file loading now defaults to "moo.cfg" in the CWD if
--config-file is not specified, and environment variables override
settings in the config file

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
