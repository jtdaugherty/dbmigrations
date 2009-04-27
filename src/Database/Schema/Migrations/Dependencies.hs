module Database.Schema.Migrations.Dependencies
    ( Dependable(..)
    )
where

import qualified Data.Map as Map

import Data.Maybe ( isJust )
import Control.Monad ( when )
import Control.Monad.State ( State, execState, get )

class (Eq a, Ord a) => Dependable a where
    -- |The identifiers of the objects on which a depends.
    depsOf :: a -> [String]
    depId :: a -> String

data DependencyGraph = DepGraph { forwardDeps :: Map.Map String [String]
                                , reverseDeps :: Map.Map String [String]
                                }

mkDepGraph :: (Dependable a) => [a] -> DependencyGraph
mkDepGraph objects = DepGraph fd rd
    where
      fd = Map.fromList [ (depId obj, depsOf obj) | obj <- objects ]
      rd = Map.fromList [ (depId obj, rDeps obj fd) | obj <- objects ]
      rDeps obj fd = [ k | (k, vs) <- Map.toList fd, depId obj `elem` vs ]

fullReverseDeps :: String -> DependencyGraph -> Maybe [String]
fullReverseDeps objId graph = do
  reverseDepList <- Map.lookup objId $ reverseDeps graph
  recursedRdeps <- mapM (\k -> fullReverseDeps k graph) reverseDepList
  thisRdeps <- Map.lookup objId $ reverseDeps graph
  return $ (concat recursedRdeps) ++ thisRdeps