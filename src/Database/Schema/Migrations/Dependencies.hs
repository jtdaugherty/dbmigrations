{-# LANGUAGE TypeSynonymInstances #-}
module Database.Schema.Migrations.Dependencies
    ( Dependable(..)
    , DependencyGraph
    , mkDepGraph
    , hasCycle
    )
where

import Data.Maybe ( fromJust )

import Data.Graph.Inductive.Graph ( Graph(..), nodes, edges, Node, suc', indeg )
import Data.Graph.Inductive.PatriciaTree ( Gr )

class (Eq a, Ord a) => Dependable a where
    -- |The identifiers of the objects on which a depends.
    depsOf :: a -> [String]
    depId :: a -> String

type DependencyGraph = Gr String String

instance Eq DependencyGraph where
    g1 == g2 = (nodes g1 == nodes g2 &&
                edges g1 == edges g2)

instance Show DependencyGraph where
    show g = "(" ++ (show $ nodes g) ++ ", " ++ (show $ edges g) ++ ")"

-- Return True if the specified graph contains a cycle.
hasCycle :: Graph g => g a b -> Bool
hasCycle g = if emptyGraph
             then False -- if the graph is empty, it clearly does not contain a cycle.
             else if length noInputs == 0
                  then True -- if the graph has no vertices with
                            -- indegree 0, it MUST contain a cycle.
                  else hasCycle' g [] [head noInputs] -- otherwise, check it.
    where
      emptyGraph = nodes g == []
      -- the vertices with no inbound edges.
      noInputs = [ v | v <- nodes g, indeg g v == 0 ]

hasCycle' :: Graph g => g a b -> [Node] -> [Node] -> Bool
hasCycle' _ _ [] = False
hasCycle' g visited (v:vs) =
    -- Look for the next vertex in the list of vertices to visit
    case match v g of
      -- If it wasn't found (should never happen!) just continue
      (Nothing, g') -> hasCycle' g' visited vs
      -- If the vertex we found has already been visited, the graph
      -- has a cycle; otherwise, add its successors to the list of
      -- vertices to visit, add the vertex to the visited list, and
      -- continue.  Only add successors to the to-visit list if they
      -- aren't already there.
      (Just c, _) -> if v `elem` visited
                      then True
                      else hasCycle' g (v:visited) ([ e | e <- suc' c, not (e `elem` vs) ] ++ vs)

mkDepGraph :: (Dependable a) => [a] -> Either String DependencyGraph
mkDepGraph objects = if hasCycle depGraph
                     then Left "Invalid dependency graph; cycle detected"
                     else Right depGraph
    where
      depGraph = mkGraph n e
      n = [ (fromJust $ lookup o ids, depId o) | o <- objects ]
      e = [ ( fromJust $ lookup o ids
            , fromJust $ lookup d ids
            , depId o ++ " -> " ++ depId d) | o <- objects, d <- depsOf' o ]
      depsOf' o = map (\i -> fromJust $ lookup i objMap) $ depsOf o

      objMap = map (\o -> (depId o, o)) objects
      ids = zip objects [1..]
