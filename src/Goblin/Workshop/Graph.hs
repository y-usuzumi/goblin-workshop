module Goblin.Workshop.Graph where

import Data.List
import Control.Monad.ST
import qualified Data.Map as M

type Vertices i a = M.Map i a
type Edges i = M.Map i (Int, [i])  -- (indegree, outnodes)

data Graph i a = Graph { vertices :: Vertices i a
                       , edges :: Edges i
                       }

initGraph :: Ord i
          => [(i, a)]  -- (key, value) pairs
          -> [(i, i)]  -- (start, end) unidirections
          -> Graph i a
initGraph kvps conns = Graph { vertices
                             , edges
                             } where
  emptyMap keys defVal = M.fromList $ map (\k -> (k, defVal)) keys
  vertices = M.fromList kvps
  edges = foldl' reducer (emptyMap (M.keys vertices) (0, [])) conns
  reducer acc curr = let
    start = fst curr
    end = snd curr
    in
    M.alter (\ends -> case ends of
                        Nothing -> Just (1, [snd curr])
                        Just (indegree, existing) -> Just (indegree+1, snd curr:existing)
            ) start acc

entryPoints :: Ord i => Graph i a -> [(i, a)]
entryPoints g@Graph{..} = M.assocs entries where
  entries = flip M.filterWithKey vertices $ \i a ->
    fst (edges M.! i) == 0
