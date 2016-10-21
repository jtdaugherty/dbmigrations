module Main
    ( main
    )
where

import Prelude

main :: IO ()
main = do
  error $
    "This package (dbmigrations) does no longer contain the executable to \
    \create, apply or revert database migrations. Please install the specific \
    \wrapper-package for your database, that is, dbmigrations-postgresql, \
    \dbmigrations-mysql, or dbmigrations-sqlite. These packages contain \
    \database specific executables (moo-postgresql, moo-mysql and moo-sqlite \
    \respectively) that replace the former moo executable from the \
    \dbmigrations package."

