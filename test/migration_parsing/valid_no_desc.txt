Created: 2009-04-15 10:02:06 UTC
Depends: another_migration
Apply:

  CREATE TABLE test (
    a int
  );

Revert: DROP TABLE test;
