{-# LANGUAGE TypeSynonymInstances,GeneralizedNewtypeDeriving,MultiParamTypeClasses #-}
module MigrationsTest
    ( tests
    )
where

import Test.HUnit
import Control.Monad.Identity ( runIdentity, Identity )
import qualified Data.Map as Map

import Database.Schema.Migrations
import Database.Schema.Migrations.Dependencies
import Database.Schema.Migrations.Migration
import Database.Schema.Migrations.Backend
import Common

tests :: [Test]
tests = missingMigrationsTests

type TestBackend = [TestDependable]

newtype TestM a = TestM (Identity a) deriving (Monad)

instance MonadMigration TestM where
    getCurrentTime = undefined

instance Backend TestBackend TestM where
    getBootstrapMigration _ = undefined
    isBootstrapped _ = return True
    applyMigration _ _ = undefined
    revertMigration _ _ = undefined
    getMigrations b = return $ map depId b

-- |Given a backend and a store, what are the list of migrations
-- missing in the backend that are available in the store?
type MissingMigrationTestCase = (TestBackend, [String], [String])

missingMigrationsTestcases :: [MissingMigrationTestCase]
missingMigrationsTestcases =  [ ([], [], [])
                              , ([TD "one" []], ["one"], [])
                              , ([TD "one" []], ["two"], ["two"])
                              ]

mkTest :: MissingMigrationTestCase -> Test
mkTest (b, sm, expected) =
  let result = runIdentity act
      TestM act = missingMigrations b s
      s = Map.fromList [ (e, undefined) | e <- sm ]
  in expected ~=? result

missingMigrationsTests :: [Test]
missingMigrationsTests = map mkTest missingMigrationsTestcases