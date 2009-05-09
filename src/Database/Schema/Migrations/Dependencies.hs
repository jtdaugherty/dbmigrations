module Database.Schema.Migrations.Dependencies
    ( Dependable(..)
    , DependencyInfo(..)
    , depInfo
    )
where

import qualified Data.Map as Map

class (Eq a, Ord a) => Dependable a where
    -- |The identifiers of the objects on which a depends.
    depsOf :: a -> [String]
    depId :: a -> String

data DependencyInfo a = DepInfo { forwardDeps :: Map.Map String [String]
                                , reverseDeps :: Map.Map String [String]
                                , contents :: Map.Map String a
                                }
                      deriving (Show, Eq)

depInfo :: (Dependable a) => [a] -> DependencyInfo a
depInfo objects = DepInfo fd rd c
    where
      c = Map.fromList [ (depId obj, obj) | obj <- objects ]
      fd = Map.fromList [ (depId obj, depsOf obj) | obj <- objects ]
      rd = Map.fromList [ (depId obj, rDeps obj fd) | obj <- objects ]
      rDeps obj deps = [ k | (k, vs) <- Map.toList deps, depId obj `elem` vs ]
