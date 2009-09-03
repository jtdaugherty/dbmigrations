{-# LANGUAGE TypeSynonymInstances,GeneralizedNewtypeDeriving,MultiParamTypeClasses #-}
module MigrationsTest
    ( tests
    )
where

import Test.HUnit
import Control.Monad.Identity ( runIdentity, Identity )
import qualified Data.Map as Map
import Data.Time.Clock ( UTCTime )

import Database.Schema.Migrations
import Database.Schema.Migrations.Store ( MigrationMap )
import Database.Schema.Migrations.Migration
import Database.Schema.Migrations.Backend

tests :: [Test]
tests = migrationsToApplyTests

type TestBackend = [Migration]

newtype TestM a = TestM (Identity a) deriving (Monad)

instance MonadMigration TestM where
    getCurrentTime = undefined

instance Backend TestBackend TestM where
    getBootstrapMigration _ = undefined
    isBootstrapped _ = return True
    applyMigration _ _ = undefined
    revertMigration _ _ = undefined
    getMigrations b = return $ map mId b

-- |Given a backend and a store, what are the list of migrations
-- missing in the backend that are available in the store?
type MissingMigrationTestCase = (MigrationMap, TestBackend, Migration,
                                 Either String [Migration])

ts :: UTCTime
ts = read "2009-04-15 10:02:06 UTC"

blankMigration :: Migration
blankMigration = Migration { mTimestamp = ts
                           , mId = undefined
                           , mDesc = Nothing
                           , mApply = ""
                           , mRevert = Nothing
                           , mDeps = []
                           }

missingMigrationsTestcases :: [MissingMigrationTestCase]
missingMigrationsTestcases =  [ (m, [], one, Right [one])
                              , (m, [one], one, Right [])
                              , (m, [one], two, Right [two])
                              , (m, [one, two], one, Right [])
                              , (m, [one, two], two, Right [])
                              ]
    where
      one = blankMigration { mId = "one" }
      two = blankMigration { mId = "two", mDeps = ["one"] }
      m = Map.fromList [ (mId e, e) | e <- [one, two] ]

mkTest :: MissingMigrationTestCase -> Test
mkTest (mapping, backend, theMigration, expected) =
  let TestM act = migrationsToApply mapping backend theMigration
      result = runIdentity act
  in expected ~=? result

migrationsToApplyTests :: [Test]
migrationsToApplyTests = map mkTest missingMigrationsTestcases