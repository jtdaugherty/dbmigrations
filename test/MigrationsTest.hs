{-# LANGUAGE TypeSynonymInstances #-}
module MigrationsTest
    ( tests
    )
where

import Test.HUnit
import Control.Monad ( sequence )

import Database.Schema.Migrations
import Database.Schema.Migrations.Dependencies
import Database.Schema.Migrations.Migration
import Database.Schema.Migrations.Backend
import Database.Schema.Migrations.Store
import Common

tests :: IO [Test]
tests = missingMigrationsTests

type TestBackend = [TestDependable]
type TestStore = [TestDependable]

instance Backend TestBackend where
    getBootstrapMigration _ = undefined
    isBootstrapped _ = return True
    applyMigration _ _ = undefined
    revertMigration _ _ = undefined
    getMigrations b = return $ map depId b

instance MigrationStore TestStore where
    getMigrations b = do
      (sequence $ map mkMigration b) >>= return
          where
            mkMigration d = do
                        m <- newMigration $ depId d
                        return $ m { mDeps = depsOf d }

-- |Given a backend and a store, what are the list of migrations
-- missing in the backend that are available in the store?
type MissingMigrationTestCase = (TestBackend, TestStore, [String])

missingMigrationsTestcases :: [MissingMigrationTestCase]
missingMigrationsTestcases =  [ ([], [], [])
                              , ([TD "one" []], [TD "one" []], [])
                              , ([TD "one" []], [TD "two" []], ["two"])
                              ]

mkTest :: MissingMigrationTestCase -> IO Test
mkTest (b, s, expected) = do
  result <- missingMigrations b s
  return $ expected ~=? result

missingMigrationsTests :: IO [Test]
missingMigrationsTests = sequence $ map mkTest missingMigrationsTestcases