{-# LANGUAGE OverloadedStrings #-}
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
        ++ validateMigrationMapTests

type ValidateSingleTestCase = ( MigrationMap
                              , Migration
                              , [MapValidationError]
                              )

type ValidateMigrationMapTestCase = ( MigrationMap
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

m1 :: Migration
m1 = noDeps { mId = "m1"
            , mDeps = [] }

m2 :: Migration
m2 = noDeps { mId = "m2"
            , mDeps = ["m1"] }

m3 :: Migration
m3 = noDeps { mId = "m3"
            , mDeps = ["nonexistent"] }

m4 :: Migration
m4 = noDeps { mId = "m4"
            , mDeps = ["one", "two"] }

map1 :: MigrationMap
map1 = Map.fromList [ ("m1", m1)
                    , ("m2", m2)
                    ]

map2 :: MigrationMap
map2 = Map.fromList [ ("m3", m3)
                    ]

map3 :: MigrationMap
map3 = Map.fromList [ ("m4", m4)
                    ]

validateMapTestCases :: [ValidateMigrationMapTestCase]
validateMapTestCases = [ (emptyMap, [])
                       , (map1, [])
                       , (map2, [DependencyReferenceError (mId m3) "nonexistent"])
                       , (map3, [ DependencyReferenceError (mId m4) "one"
                                , DependencyReferenceError (mId m4) "two"])
                       ]

validateMigrationMapTests :: [Test]
validateMigrationMapTests =
    map mkValidateMapTest validateMapTestCases
        where
          mkValidateMapTest (mmap, errs) =
              errs ~=? validateMigrationMap mmap
