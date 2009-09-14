module StoreTest
    ( tests
    )
where

import Test.HUnit
import qualified Data.Map as Map

import Database.Schema.Migrations.Migration
import Database.Schema.Migrations.Store

tests :: [Test]
tests = validateSingleMigrationTests

type ValidateSingleTestCase = ( MigrationMap
                              , Migration
                              , [MapValidationError]
                              )

emptyMap :: MigrationMap
emptyMap = Map.fromList []

partialMap :: MigrationMap
partialMap = Map.fromList [ ("one", undefined)
                          , ("three", undefined)
                          ]

fullMap :: MigrationMap
fullMap = Map.fromList [ ("one", undefined)
                       , ("two", undefined)
                       , ("three", undefined)
                       ]

withDeps :: Migration
withDeps = Migration { mTimestamp = undefined
                     , mId = "with_deps"
                     , mDesc = Just "with dependencies"
                     , mApply = ""
                     , mRevert = Nothing
                     , mDeps = ["one", "two", "three"]
                     }

noDeps :: Migration
noDeps = Migration { mTimestamp = undefined
                   , mId = "no_deps"
                   , mDesc = Just "no dependencies"
                   , mApply = ""
                   , mRevert = Nothing
                   , mDeps = []
                   }

validateSingleTestCases :: [ValidateSingleTestCase]
validateSingleTestCases = [ (emptyMap, withDeps, [ DependencyReferenceError (mId withDeps) "one"
                                                 , DependencyReferenceError (mId withDeps) "two"
                                                 , DependencyReferenceError (mId withDeps) "three"
                                                 ]
                            )
                          , (emptyMap, noDeps, [])
                          , (partialMap, withDeps, [DependencyReferenceError (mId withDeps) "two"])
                          , (fullMap, withDeps, [])
                          , (fullMap, noDeps, [])
                          ]

validateSingleMigrationTests :: [Test]
validateSingleMigrationTests =
    map mkValidateSingleTest validateSingleTestCases
        where
          mkValidateSingleTest (mmap, m, errs) =
              errs ~=? validateSingleMigration mmap m