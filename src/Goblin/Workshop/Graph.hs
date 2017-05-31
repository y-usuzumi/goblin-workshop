{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Goblin.Workshop.Graph where

import Control.Monad.ST
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

data Graph i a = Graph { vertices :: V.Vector a
                       , edges :: V.Vector [Int]  -- Unidirectional index to index
                       , iamap :: M.Map i Int
                       }

connect :: Ord i => Graph i a -> i -> i -> Graph i a
connect g@Graph{..} sp ep = let
  spIdx = iamap M.! sp
  epIdx = iamap M.! ep
  -- See https://ghc.haskell.org/trac/ghc/wiki/ImpredicativePolymorphism
  -- Due to no support for impredicative polymorphism
  -- I can't write `flip V.modify edges $ \mv -> ...`
  -- Basically one can't put a `forall` polymorphism type on a type variable
  edges' = V.modify (\mv -> MV.modify mv (epIdx:) spIdx) edges
  in
  g { edges=edges' }
