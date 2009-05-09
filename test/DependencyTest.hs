module DependencyTest
    ( tests
    )
where

import Test.HUnit
import qualified Data.Map as Map

import Database.Schema.Migrations.Dependencies

tests :: [Test]
tests = depInfoTests

data TestDependable = TD { tdId :: String
                         , tdDeps :: [String]
                         }
                      deriving (Show, Eq, Ord)

instance Dependable TestDependable where
    depId = tdId
    depsOf = tdDeps

type DepInfoTestCase = ([TestDependable], DependencyInfo TestDependable)

depInfoTestCases :: [DepInfoTestCase]
depInfoTestCases = [ ( []
                     , DepInfo (Map.fromList []) (Map.fromList []) (Map.fromList [])
                     )
                   , ( [first, second]
                     , DepInfo (Map.fromList [("first", []), ("second", ["first"])])
                                   (Map.fromList [("second", []), ("first", ["second"])])
                                   (Map.fromList [ ("first", first)
                                                 , ("second", second)
                                                 ]
                                   )
                     )
                   ]
    where
      first = TD "first" []
      second = TD "second" ["first"]
depInfoTests :: [Test]
depInfoTests = map mkDepInfoTest depInfoTestCases

mkDepInfoTest :: DepInfoTestCase -> Test
mkDepInfoTest (input, expected) = expected ~=? depInfo input