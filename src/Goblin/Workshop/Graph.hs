module Goblin.Workshop.Graph where

import           Control.Monad.ST
import           Data.List
import qualified Data.Map         as M
import qualified Data.Set         as S

type Vertices i a = M.Map i a
type Edges i = M.Map i (S.Set i)

data Graph i a = Graph { vertices    :: Vertices i a
                       , outEdgesMap :: Edges i
                       , inEdgesMap  :: Edges i
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
  emptyEdgesMap = M.map (\k -> S.empty) vertices
  (outEdgesMap, inEdgesMap) = foldl' reducer ( emptyEdgesMap
                                             , emptyEdgesMap
                                             ) conns
  reducer (outEdges, inEdges) (start, end) = let
    -- TODO: Check for existence of start and end
    outEdgesMap' = M.adjust (S.insert end) start outEdges
    inEdgesMap' = M.adjust (S.insert start) end inEdges
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
      reducer acc curr = M.adjust (S.delete i) curr acc

removeEntryPoints :: Ord i => [i] -> Graph i a -> Graph i a
removeEntryPoints points g@Graph{..} = let
  points' = filter ((== 0) . length . (inEdgesMap M.!)) points
  in
  foldl' (flip removeEntryPoint) g points'
