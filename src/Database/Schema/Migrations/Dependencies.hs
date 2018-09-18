{-# LANGUAGE TypeSynonymInstances #-}
-- |This module types and functions for representing a dependency
-- graph of arbitrary objects and functions for querying such graphs
-- to get dependency and reverse dependency information.
module Database.Schema.Migrations.Dependencies
    ( Dependable(..)
    , DependencyGraph(..)
    , mkDepGraph
    , dependencies
    , reverseDependencies
    )
where

import Data.Text ( Text )
import Data.Maybe ( fromJust )
import Data.Monoid ( (<>) )
import Data.Graph.Inductive.Graph ( Graph(..), nodes, edges, Node, suc, pre, lab )
import Data.Graph.Inductive.PatriciaTree ( Gr )

import Database.Schema.Migrations.CycleDetection ( hasCycle )

-- |'Dependable' objects supply a representation of their identifiers,
-- and a list of other objects upon which they depend.
class (Eq a, Ord a) => Dependable a where
    -- |The identifiers of the objects on which @a@ depends.
    depsOf :: a -> [Text]
    -- |The identifier of a 'Dependable' object.
    depId :: a -> Text

-- |A 'DependencyGraph' represents a collection of objects together
-- with a graph of their dependency relationships.  This is intended
-- to be used with instances of 'Dependable'.
data DependencyGraph a = DG { depGraphObjectMap :: [(a, Int)]
                            -- ^ A mapping of 'Dependable' objects to
                            -- their graph vertex indices.
                            , depGraphNameMap :: [(Text, Int)]
                            -- ^ A mapping of 'Dependable' object
                            -- identifiers to their graph vertex
                            -- indices.
                            , depGraph :: Gr Text Text
                            -- ^ A directed 'Gr' (graph) of the
                            -- 'Dependable' objects' dependency
                            -- relationships, with 'Text' vertex and
                            -- edge labels.
                            }

instance (Eq a) => Eq (DependencyGraph a) where
    g1 == g2 = ((nodes $ depGraph g1) == (nodes $ depGraph g2) &&
                (edges $ depGraph g1) == (edges $ depGraph g2))

instance (Show a) => Show (DependencyGraph a) where
    show g = "(" ++ (show $ nodes $ depGraph g) ++ ", " ++ (show $ edges $ depGraph g) ++ ")"

-- XXX: provide details about detected cycles
-- |Build a dependency graph from a list of 'Dependable's.  Return the
-- graph on success or return an error message if the graph cannot be
-- constructed (e.g., if the graph contains a cycle).
mkDepGraph :: (Dependable a) => [a] -> Either String (DependencyGraph a)
mkDepGraph objects = if hasCycle theGraph
                     then Left "Invalid dependency graph; cycle detected"
                     else Right $ DG { depGraphObjectMap = ids
                                     , depGraphNameMap = names
                                     , depGraph = theGraph
                                     }
    where
      theGraph = mkGraph n e
      n = [ (fromJust $ lookup o ids, depId o) | o <- objects ]
      e = [ ( fromJust $ lookup o ids
            , fromJust $ lookup d ids
            , depId o <> " -> " <> depId d) | o <- objects, d <- depsOf' o ]
      depsOf' o = map (\i -> fromJust $ lookup i objMap) $ depsOf o

      objMap = map (\o -> (depId o, o)) objects
      ids = zip objects [1..]
      names = map (\(o,i) -> (depId o, i)) ids

type NextNodesFunc = Gr Text Text -> Node -> [Node]

cleanLDups :: (Eq a) => [a] -> [a]
cleanLDups [] = []
cleanLDups [e] = [e]
cleanLDups (e:es) = if e `elem` es then (cleanLDups es) else (e:cleanLDups es)

-- |Given a dependency graph and an ID, return the IDs of objects that
-- the object depends on.  IDs are returned with least direct
-- dependencies first (i.e., the apply order).
dependencies :: (Dependable d) => DependencyGraph d -> Text -> [Text]
dependencies g m = reverse $ cleanLDups $ dependenciesWith suc g m

-- |Given a dependency graph and an ID, return the IDs of objects that
-- depend on it.  IDs are returned with least direct reverse
-- dependencies first (i.e., the revert order).
reverseDependencies :: (Dependable d) => DependencyGraph d -> Text -> [Text]
reverseDependencies g m = reverse $ cleanLDups $ dependenciesWith pre g m

dependenciesWith :: (Dependable d) => NextNodesFunc -> DependencyGraph d -> Text -> [Text]
dependenciesWith nextNodes dg@(DG _ nMap theGraph) name =
    let lookupId = fromJust $ lookup name nMap
        depNodes = nextNodes theGraph lookupId
        recurse theNodes = map (dependenciesWith nextNodes dg) theNodes
        getLabel node = fromJust $ lab theGraph node
        labels = map getLabel depNodes
    in labels ++ (concat $ recurse labels)
