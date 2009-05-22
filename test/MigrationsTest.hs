{-# LANGUAGE TypeSynonymInstances,GeneralizedNewtypeDeriving,MultiParamTypeClasses #-}
module MigrationsTest
    ( tests
    )
where

import Test.HUnit
import Control.Monad.Identity ( runIdentity, Identity )

import Database.Schema.Migrations
import Database.Schema.Migrations.Dependencies
import Database.Schema.Migrations.Migration
import Database.Schema.Migrations.Backend
import Database.Schema.Migrations.Store
import Common

tests :: [Test]
tests = missingMigrationsTests

type TestBackend = [TestDependable]
type TestStore = [TestDependable]

newtype TestM a = TestM (Identity a) deriving (Monad)

instance MonadMigration TestM where
    getCurrentTime = undefined

instance Backend TestBackend TestM where
    getBootstrapMigration _ = undefined
    isBootstrapped _ = return True
    applyMigration _ _ = undefined
    revertMigration _ _ = undefined
    getMigrations b = return $ map depId b

instance MigrationStore TestStore TestM where
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

mkTest :: MissingMigrationTestCase -> Test
mkTest (b, s, expected) =
  let result = runIdentity act
      TestM act = missingMigrations b s
  in expected ~=? result

missingMigrationsTests :: [Test]
missingMigrationsTests = map mkTest missingMigrationsTestcases