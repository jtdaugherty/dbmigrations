module DependencyTest
    ( tests
    )
where

import Test.HUnit
import Data.Graph.Inductive.Graph ( Graph(..) )

import Database.Schema.Migrations.Dependencies
import Common

tests :: [Test]
tests = depGraphTests ++ dependencyTests

type DepGraphTestCase = ([TestDependable], Either String (DependencyGraph TestDependable))

depGraphTestCases :: [DepGraphTestCase]
depGraphTestCases = [ ( []
                     , Right $ DG [] [] empty
                      )
                    , ( [first, second]
                      , Right $ DG [(first,1),(second,2)]
                                  [("first",1),("second",2)] (mkGraph [(1, "first"), (2, "second")]
                                                              [(2, 1, "first -> second")])
                      )
                    , ( [cycleFirst, cycleSecond]
                      , Left "Invalid dependency graph; cycle detected")
                    ]
    where
      first = TD "first" []
      second = TD "second" ["first"]
      cycleFirst = TD "first" ["second"]
      cycleSecond = TD "second" ["first"]

depGraphTests :: [Test]
depGraphTests = map mkDepGraphTest depGraphTestCases

mkDepGraphTest :: DepGraphTestCase -> Test
mkDepGraphTest (input, expected) = expected ~=? mkDepGraph input

data Direction = Forward | Reverse deriving (Show)
type DependencyTestCase = ([TestDependable], String, Direction, [String])

dependencyTestCases :: [DependencyTestCase]
dependencyTestCases = [ ([TD "first" []], "first", Forward, [])
                      , ([TD "first" []], "first", Reverse, [])

                      , ([TD "first" ["second"], TD "second" []], "first", Forward, ["second"])
                      , ([TD "first" ["second"], TD "second" []], "second", Reverse, ["first"])
                      , ([TD "first" ["second"], TD "second" ["third"], TD "third" []], "first", Forward, ["third", "second"])
                      , ([TD "first" ["second"], TD "second" ["third"], TD "third" [], TD "fourth" ["third"]]
                        , "first", Forward, ["third", "second"])
                      , ([TD "first" [], TD "second" ["first"]]
                        , "first", Reverse, ["second"])
                      , ([TD "first" [], TD "second" ["first"], TD "third" ["second"]]
                        , "first", Reverse, ["third", "second"])
                      , ([TD "first" [], TD "second" ["first"], TD "third" ["second"], TD "fourth" ["second"]]
                        , "first", Reverse, ["fourth", "third", "second"])
                      , ([ TD "first" ["second"], TD "second" ["third"], TD "third" ["fourth"]
                         , TD "second" ["fifth"], TD "fifth" ["third"], TD "fourth" []]
                        , "fourth", Reverse, ["first", "second", "fifth", "third"])
                      , ([ TD "first" ["second"], TD "second" ["third", "fifth"], TD "third" ["fourth"]
                         , TD "fifth" ["third"], TD "fourth" []]
                        , "first", Forward, ["fourth", "third", "fifth", "second"])
                      ]

fromRight :: Either a b -> b
fromRight (Left _) = error "Got a Left value"
fromRight (Right v) = v

mkDependencyTest :: DependencyTestCase -> Test
mkDependencyTest testCase@(deps, a, dir, expected) =
    let f = case dir of
              Forward -> dependencies
              Reverse -> reverseDependencies
    in (show testCase) ~: expected ~=? f (fromRight $ mkDepGraph deps) a

dependencyTests :: [Test]
dependencyTests = map mkDependencyTest dependencyTestCases