module DependencyTest
    ( tests
    )
where

import Test.HUnit
import Data.Graph.Inductive.Graph ( Graph(..) )
import Data.Graph.Inductive.PatriciaTree ( Gr )

import Database.Schema.Migrations.Dependencies

tests :: [Test]
tests = depGraphTests ++ mkCycleTests ++ dependencyTests

data TestDependable = TD { tdId :: String
                         , tdDeps :: [String]
                         }
                      deriving (Show, Eq, Ord)

instance Dependable TestDependable where
    depId = tdId
    depsOf = tdDeps

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

noCycles :: Gr String String
noCycles = mkGraph [(1,"one"),(2,"two")] [(1,2,"one->two")]

noCyclesEmpty :: Gr String String
noCyclesEmpty = mkGraph [] []

withCycleSimple :: Gr String String
withCycleSimple = mkGraph [(1,"one")] [(1,1,"one->one")]

withCycleComplex :: Gr String String
withCycleComplex = mkGraph [(1,"one"),(2,"two"),(3,"three"),(4,"four")]
                     [(4,1,"four->one"),(1,2,"one->two"),(2,3,"two->three"),(3,1,"three->one")]

withCycleRadial :: Gr String String
withCycleRadial = mkGraph [(1,"one"),(2,"two"),(3,"three"),(4,"four")]
                     [(2,1,""),(2,3,""),(3,4,""),(3,2,"")]

noCycleRadial :: Gr String String
noCycleRadial = mkGraph [(1,""),(2,""),(3,""),(4,"")]
                  [(1,2,""),(3,1,""),(4,1,"")]

type CycleTestCase = (Gr String String, Bool)

cycleTests :: [CycleTestCase]
cycleTests = [ (noCyclesEmpty, False)
             , (noCycles, False)
             , (noCycleRadial, False)
             , (withCycleSimple, True)
             , (withCycleComplex, True)
             , (withCycleRadial, True)
             ]

mkCycleTests :: [Test]
mkCycleTests = map mkCycleTest cycleTests
    where
      mkCycleTest (g, expected) = expected ~=? hasCycle g

data Direction = Forward | Reverse deriving (Show)
type DependencyTestCase = ([TestDependable], String, Direction, [String])

dependencyTestCases :: [DependencyTestCase]
dependencyTestCases = [ ([TD "first" []], "first", Forward, [])
                      , ([TD "first" []], "first", Reverse, [])

                      , ([TD "first" ["second"], TD "second" []], "first", Forward, ["second"])
                      , ([TD "first" ["second"], TD "second" []], "second", Reverse, ["first"])
                      , ([TD "first" ["second"], TD "second" ["third"], TD "third" []], "first", Forward, ["second", "third"])
                      , ([TD "first" ["second"], TD "second" ["third"], TD "third" [], TD "fourth" ["third"]]
                        , "first", Forward, ["second", "third"])
                      , ([TD "first" ["second"], TD "second" ["third"], TD "third" [], TD "fourth" ["third"]]
                        , "third", Reverse, ["second", "fourth", "first"])
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