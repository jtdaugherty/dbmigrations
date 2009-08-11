module CycleDetectionTest
    ( tests
    )
where

import Test.HUnit
import Data.Graph.Inductive.PatriciaTree ( Gr )
import Data.Graph.Inductive.Graph ( mkGraph )

import Database.Schema.Migrations.CycleDetection

tests :: [Test]
tests = mkCycleTests

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

-- This graph would contain a loop if it were undirected, but it does
-- not contain a directed cycle.
noDirectedCycle1 :: Gr String String
noDirectedCycle1 = mkGraph [(1,""),(2,""),(3,""),(4,"")]
                   [(1,2,""),(1,3,""),(3,2,""),(2,4,"")]

-- This graph would contain a loop if it were undirected, but it does
-- not contain a directed cycle.
noDirectedCycle2 :: Gr String String
noDirectedCycle2 = mkGraph [(1,"flub"),(2,"test.db"),(3,"test2"),(4,"test3"),(5,"test1")]
                   [ (1,2,"flub->test.db")
                   , (2,3,"test.db->test2")
                   , (2,4,"test.db->test3")
                   , (3,5,"test2->test1")
                   , (4,3,"test3->test2")
                   ]

type CycleTestCase = (Gr String String, Bool)

cycleTests :: [CycleTestCase]
cycleTests = [ (noCyclesEmpty, False)
             , (noCycles, False)
             , (noCycleRadial, False)
             , (withCycleSimple, True)
             , (withCycleComplex, True)
             , (withCycleRadial, True)
             , (noDirectedCycle1, False)
             , (noDirectedCycle2, False)
             ]

mkCycleTests :: [Test]
mkCycleTests = map mkCycleTest cycleTests
    where
      mkCycleTest (g, expected) = expected ~=? hasCycle g
