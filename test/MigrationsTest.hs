{-# LANGUAGE TypeSynonymInstances,GeneralizedNewtypeDeriving,MultiParamTypeClasses,FlexibleInstances #-}
module MigrationsTest
    ( tests
    )
where

import Test.HUnit
import Control.Applicative ((<$>))
import qualified Data.Map as Map
import Data.Time.Clock ( UTCTime )

import Database.Schema.Migrations
import Database.Schema.Migrations.Store hiding (getMigrations)
import Database.Schema.Migrations.Migration
import Database.Schema.Migrations.Backend

tests :: [Test]
tests = migrationsToApplyTests

testBackend :: [Migration] -> Backend
testBackend testMs =
    Backend { getBootstrapMigration = undefined
            , isBootstrapped = return True
            , applyMigration = const undefined
            , revertMigration = const undefined
            , getMigrations = return $ mId <$> testMs
            , commitBackend = return ()
            , rollbackBackend = return ()
            , disconnectBackend = return ()
            }

-- |Given a backend and a store, what are the list of migrations
-- missing in the backend that are available in the store?
type MissingMigrationTestCase = (MigrationMap, Backend, Migration,
                                 [Migration])

ts :: UTCTime
ts = read "2009-04-15 10:02:06 UTC"

blankMigration :: Migration
blankMigration = Migration { mTimestamp = Just ts
                           , mId = undefined
                           , mDesc = Nothing
                           , mApply = ""
                           , mRevert = Nothing
                           , mDeps = []
                           }

missingMigrationsTestcases :: [MissingMigrationTestCase]
missingMigrationsTestcases =  [ (m, testBackend [], one, [one])
                              , (m, testBackend [one], one, [])
                              , (m, testBackend [one], two, [two])
                              , (m, testBackend [one, two], one, [])
                              , (m, testBackend [one, two], two, [])
                              ]
    where
      one = blankMigration { mId = "one" }
      two = blankMigration { mId = "two", mDeps = ["one"] }
      m = Map.fromList [ (mId e, e) | e <- [one, two] ]

mkTest :: MissingMigrationTestCase -> Test
mkTest (mapping, backend, theMigration, expected) =
  let Right graph = depGraphFromMapping mapping
      storeData = StoreData mapping graph
      result = migrationsToApply storeData backend theMigration
  in "a test" ~: do
      actual <- result
      return $ expected == actual

migrationsToApplyTests :: [Test]
migrationsToApplyTests = map mkTest missingMigrationsTestcases
