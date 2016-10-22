
dbmigrations
------------

This package contains a library for the creation, management, and
installation of schema updates (called "migrations") for a relational
database. In particular, this package lets the migration author express
explicit dependencies between migrations. This library is accompanied
by a number database-specific packages that contain the management
tools to automatically install or revert migrations accordingly.

This package operates on two logical entities:

 - The "backend": the relational database whose schema you want to
   manage.

 - The "migration store": the collection of schema changes you want to
   apply to the database. These migrations are expressed using plain
   text files collected together in a single directory, although the
   library is general enough to permit easy implementation of other
   storage representations for migrations.

Getting started
---------------

Users should probably not use this package directly but rather install
and use the database-specific package, like dbmigrations-postgresql,
dbmigrations-mysql or dbmigrations-sqlite. These packages include
backend-specific variants of the "moo" management program
("moo-postgresql", "moo-mysql", and "moo-sqlite" respectively). See
MOO.TXT for details on how to use these tools to manage your database
migrations.

Submitting patches
------------------

I'll gladly consider accepting patches to this package; please do not
hesitate to submit GitHub pull requests. I'll be more likely to accept
a patch if you can follow these guidelines where appropriate:

  - Keep patches small; a single patch should make a single logical
    change with minimal scope.

  - If possible, include tests with your patch.

  - If possible, include haddock with your patch.
