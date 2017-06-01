module Goblin.Workshop.Graph where

import Debug.Trace
import Data.List
import Control.Monad.ST
import qualified Data.Map as M

type Vertices i a = M.Map i a
type Edges i = M.Map i [i]

data Graph i a = Graph { vertices :: Vertices i a
                       , outEdgesMap :: Edges i
                       , inEdgesMap :: Edges i
                       } deriving (Eq, Show)

initGraph :: Ord i
          => [(i, a)]  -- (key, value) pairs
          -> [(i, i)]  -- (start, end) unidirections
          -> Graph i a
initGraph kvps conns = Graph { vertices
                             , outEdgesMap
                             , inEdgesMap
                             } where
  vertices = M.fromList kvps
  emptyEdgesMap = M.fromList $ map (\k -> (k, [])) (M.keys vertices)
  (outEdgesMap, inEdgesMap) = foldl' reducer ( emptyEdgesMap
                                             , emptyEdgesMap
                                             ) conns
  reducer (outEdges, inEdges) curr = let
    start = fst curr
    end = snd curr
    outEdgesMap' = M.adjust ( \ existing ->
                                reverse (end:reverse existing)
                            ) start outEdges
    inEdgesMap' = M.adjust ( \ existing ->
                               reverse (start:reverse existing)
                           ) end inEdges
    in (outEdgesMap', inEdgesMap')

entryPoints :: Ord i => Graph i a -> [(i, a)]
entryPoints g@Graph{..} = M.assocs entries where
  entries = flip M.filterWithKey vertices $ \i a ->
    length (inEdgesMap M.! i) == 0

removeEntryPoint :: Ord i => i -> Graph i a -> Graph i a
removeEntryPoint i g@Graph{..} = let
  outEdges = outEdgesMap M.! i
  inEdges = inEdgesMap M.! i
  in
  if length inEdges /= 0 then g
  else let
    vertices' = M.delete i vertices
    outEdgesMap' = M.delete i outEdgesMap
    inEdgesMap' = M.delete i inEdgesMap
    outEdgesMap'' = foldl' reducer outEdgesMap' inEdges
    inEdgesMap'' = foldl' reducer inEdgesMap' outEdges
    in
    Graph { vertices = vertices'
          , outEdgesMap = outEdgesMap''
          , inEdgesMap = inEdgesMap''
          }
    where
      reducer acc curr = M.adjust (filter (/= i)) curr acc

removeEntryPoints :: Ord i => [i] -> Graph i a -> Graph i a
removeEntryPoints points g@Graph{..} = let
  points' = filter ((== 0) . length . (inEdgesMap M.!)) points
  in
  foldl' (flip removeEntryPoint) g points'
