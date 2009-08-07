{-# LANGUAGE TypeSynonymInstances #-}
module Database.Schema.Migrations.Dependencies
    ( Dependable(..)
    , DependencyGraph(..)
    , mkDepGraph
    , hasCycle
    , dependencies
    , reverseDependencies
    )
where

import Data.Maybe ( fromJust )
import Data.List ( findIndex )

import Control.Monad.State ( State, evalState, gets, get, put )
import Control.Monad ( forM )

import Data.Graph.Inductive.Graph ( Graph(..), nodes, edges, Node, suc, pre, lab )
import Data.Graph.Inductive.PatriciaTree ( Gr )

-- |Dependable objects supply a representation of their identifiers,
-- and a list of other objects upon which they depend.
class (Eq a, Ord a) => Dependable a where
    -- |The identifiers of the objects on which 'a' depends.
    depsOf :: a -> [String]
    -- |The identifier of a Dependable object.
    depId :: a -> String

-- |A DependencyGraph represents a collection of objects together with
-- a graph of their dependency relationships.
data DependencyGraph a = DG { objectMap :: [(a, Int)] -- Map from object to its graph index
                            , nameMap :: [(String, Int)] -- Map from object depid to its graph index
                            , graph :: Gr String String
                            }

instance (Eq a) => Eq (DependencyGraph a) where
    g1 == g2 = ((nodes $ graph g1) == (nodes $ graph g2) &&
                (edges $ graph g1) == (edges $ graph g2))

instance (Show a) => Show (DependencyGraph a) where
    show g = "(" ++ (show $ nodes $ graph g) ++ ", " ++ (show $ edges $ graph g) ++ ")"

data Mark = White | Gray | Black
type CycleDetectionState = [(Node, Mark)]

-- Cycle detection algorithm taken from http://www.cs.berkeley.edu/~kamil/teaching/sp03/041403.pdf
hasCycle :: Graph g => g a b -> Bool
hasCycle g = evalState (hasCycle' g) [(n, White) | n <- nodes g]

getMark :: Int -> State CycleDetectionState Mark
getMark n = gets (fromJust . lookup n) >>= return

replace :: [a] -> Int -> a -> [a]
replace elems index val
    | index > length elems = error "replacement index too large"
    | otherwise = (take index elems) ++
                  [val] ++
                  (reverse $ take ((length elems) - (index + 1)) $ reverse elems)

setMark :: Int -> Mark -> State CycleDetectionState ()
setMark n mark = do
  st <- get
  let index = fromJust $ findIndex (\(n', _) -> n' == n) st
  put $ replace st index (n, mark)

hasCycle' :: Graph g => g a b -> State CycleDetectionState Bool
hasCycle' g = do
  result <- forM (nodes g) $ \n -> do
                   m <- getMark n
                   case m of
                     White -> visit g n
                     _ -> return False
  return $ or result

visit :: Graph g => g a b -> Node -> State CycleDetectionState Bool
visit g n = do
  setMark n Gray
  result <- forM [ v | (u,v) <- edges g, u == n ] $ \node -> do
              m <- getMark node
              case m of
                Gray -> return True
                White -> visit g node
                _ -> return False
  case or result of
    True -> return True
    False -> do
              setMark n Black
              return False

-- XXX: provide details about detected cycles
mkDepGraph :: (Dependable a) => [a] -> Either String (DependencyGraph a)
mkDepGraph objects = if hasCycle depGraph
                     then Left "Invalid dependency graph; cycle detected"
                     else Right $ DG { objectMap = ids, graph = depGraph, nameMap = names }
    where
      depGraph = mkGraph n e
      n = [ (fromJust $ lookup o ids, depId o) | o <- objects ]
      e = [ ( fromJust $ lookup o ids
            , fromJust $ lookup d ids
            , depId o ++ " -> " ++ depId d) | o <- objects, d <- depsOf' o ]
      depsOf' o = map (\i -> fromJust $ lookup i objMap) $ depsOf o

      objMap = map (\o -> (depId o, o)) objects
      ids = zip objects [1..]
      names = map (\(o,i) -> (depId o, i)) ids

type NextNodesFunc = Gr String String -> Node -> [Node]

cleanLDups :: (Eq a) => [a] -> [a]
cleanLDups [] = []
cleanLDups [e] = [e]
cleanLDups (e:es) = if e `elem` es then (cleanLDups es) else (e:cleanLDups es)

-- |Given a dependency graph and an ID, return the IDs of objects that
-- the object depends on.  IDs are returned with least direct
-- dependencies first (i.e., the apply order).
dependencies :: (Dependable d) => DependencyGraph d -> String -> [String]
dependencies g m = reverse $ cleanLDups $ dependenciesWith suc g m

-- |Given a dependency graph and an ID, return the IDs of objects that
-- depend on it.  IDs are returned with least direct reverse
-- dependencies first (i.e., the revert order).
reverseDependencies :: (Dependable d) => DependencyGraph d -> String -> [String]
reverseDependencies g m = reverse $ cleanLDups $ dependenciesWith pre g m

dependenciesWith :: (Dependable d) => NextNodesFunc -> DependencyGraph d -> String -> [String]
dependenciesWith nextNodes dg@(DG _ nMap theGraph) name =
    let lookupId = fromJust $ lookup name nMap
        depNodes = nextNodes theGraph lookupId
        recurse theNodes = map (dependenciesWith nextNodes dg) theNodes
        getLabel node = fromJust $ lab theGraph node
        labels = map getLabel depNodes
    in labels ++ (concat $ recurse labels)
