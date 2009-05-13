module DependencyTest
    ( tests
    )
where

import Test.HUnit
import Data.Graph.Inductive.Graph ( Graph(..) )

import Database.Schema.Migrations.Dependencies

tests :: [Test]
tests = depGraphTests

data TestDependable = TD { tdId :: String
                         , tdDeps :: [String]
                         }
                      deriving (Show, Eq, Ord)

instance Dependable TestDependable where
    depId = tdId
    depsOf = tdDeps

type DepGraphTestCase = ([TestDependable], Either String DependencyGraph)

depGraphTestCases :: [DepGraphTestCase]
depGraphTestCases = [ ( []
                     , Right $ empty
                      )
                    , ( [first, second]
                      , Right $ mkGraph [(1, "first"), (2, "second")]
                                  [(2, 1, "first -> second")]
                      )
                    ]
    where
      first = TD "first" []
      second = TD "second" ["first"]

depGraphTests :: [Test]
depGraphTests = map mkDepGraphTest depGraphTestCases

mkDepGraphTest :: DepGraphTestCase -> Test
mkDepGraphTest (input, expected) = expected ~=? mkDepGraph input