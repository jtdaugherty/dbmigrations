module Database.Schema.Migrations.CycleDetection
    ( hasCycle
    )
where

import Data.Graph.Inductive.Graph
    ( Graph(..)
    , Node
    , nodes
    , edges
    )

import Control.Monad.State ( State, evalState, gets, get, put )
import Control.Monad ( forM )

import Data.Maybe ( fromJust )
import Data.List ( findIndex )

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
