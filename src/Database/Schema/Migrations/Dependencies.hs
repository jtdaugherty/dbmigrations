{-# LANGUAGE TypeSynonymInstances #-}
module Database.Schema.Migrations.Dependencies
    ( Dependable(..)
    , DependencyGraph
    , mkDepGraph
    )
where

import Data.Maybe ( fromJust )

import Data.Graph.Inductive.Graph ( Graph(..), nodes, edges )
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

mkDepGraph :: (Dependable a) => [a] -> DependencyGraph
mkDepGraph objects = mkGraph n e
    where
      n = [ (fromJust $ lookup o ids, depId o) | o <- objects ]
      e = [ ( fromJust $ lookup o ids
            , fromJust $ lookup d ids
            , depId o ++ " -> " ++ depId d) | o <- objects, d <- depsOf' o ]
      depsOf' o = map (\i -> fromJust $ lookup i objMap) $ depsOf o

      objMap = map (\o -> (depId o, o)) objects
      ids = zip objects [1..]
