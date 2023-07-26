{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graph where

import Control.Monad (foldM)
import Control.Monad.ST.Strict
import Control.Monad.State.Lazy
import GHC.Arr (STArray, array, newSTArray, readSTArray, thawSTArray, writeSTArray)
-- import qualified Data.Array as Array 
import Data.Array (Array, array, assocs, (//))
--import Data.Array.ST
import Data.Foldable (foldrM, foldlM)
import Data.IntMap.Lazy (IntMap, foldlWithKey, fromList, keys, (!))
import Data.Ix
import Data.List (delete)
import Data.Map.Lazy (Map, toList)
import qualified Data.Map.Lazy as Map
import qualified Data.Array ((!))
--import GHC.Arr -- (STArray, readSTArray, thawSTArray, writeSTArray)
import System.IO ()

{-
import GHC.ST
import Control.Monad.ST.Strict

bar :: Control.Monad.ST.Strict.ST Int Int
bar = _

yoo :: GHC.ST.ST Int Int
yoo = bar -}

type Node = Int

type Graph a b = IntMap (a, [(b, Node)])

data Tree a b = Nd Node a [(b, Tree a b)]
  deriving (Show)

newtype Edge = Edge (Node, Node)
  deriving (Eq, Ord, Ix, Show)

type EdgeMap b = Map Edge [b]

g :: Graph () String
g =
  fromList
    [ (1, ((), [("a", 2), ("a", 2), ("b", 3)])),
      (2, ((), [("d", 4), ("e", 5)])),
      (3, ((), [("c", 2)])),
      (4, ((), [])),
      (5, ((), [("f", 3)]))
    ]

graphBounds :: Graph a b -> (Node, Node)
graphBounds gr =
  let k = keys gr
   in (minimum k, maximum k)

paths :: Graph a b -> Node -> Tree a b
paths gr n =
  let (x, nodes) = gr ! n
      children = fmap (\(b, n) -> (b, paths gr n)) nodes
   in Nd n x children
{-
edgeMap :: (Ord b) => Graph a b -> EdgeMap b
edgeMap gr =
  let g m acc (x, n) = Map.insertWith (++) (Edge (m, n)) [x] acc
      f acc m (_, edges) = foldl (g m) acc edges
   in foldlWithKey f Map.empty gr

edgeArr :: forall a b. (Ord b) => Graph a b -> Array Edge [b]
edgeArr gr =
  let (nodeMin, nodeMax) = graphBounds gr
      em = edgeMap gr
   in array
        (Edge (nodeMin, nodeMin), Edge (nodeMax, nodeMax))
        [ (Edge (i, j), maybe [] (\x -> x) (Map.lookup (Edge (i, j)) em))
          | i <- range (nodeMin, nodeMax),
            j <- range (nodeMin, nodeMax)
        ]
-}

vertexArr :: Graph a b -> Array Node Bool
vertexArr gr =
  let (nodeMin, nodeMax) = graphBounds gr
   in array (nodeMin, nodeMax) [(i, True) | i <- range (nodeMin, nodeMax)]

getNode :: Tree a b -> Node
getNode (Nd n _ _) = n

prune :: forall a b. (Show b, Eq b) => Graph a b -> Node -> Tree a b
prune gr n = evalState (aux (paths gr n)) (vertexArr gr)
  where
    aux :: (Show b, Eq b) => Tree a b -> State (Array Node Bool) (Tree a b)
    aux (Nd n x c) = do
      em <- get
      if (Data.Array.!) em n then do
        _ <- put (em // [(n, False)])
        let f acc (x, t) = do
              children <- aux t
              return (acc ++ [(x, children)])
              
        c' <- foldM f [] c
        return (Nd n x c')
      else
        return (Nd n x [])
{-
prune' :: forall a b. (Ord b, Show b) => Graph a b -> Node -> Tree a b
prune' gr n = runST $ do
  mArr <- thawSTArray (edgeArr gr)
  aux (paths gr n) mArr
  where
    aux :: (Ord b, Show b) => forall s. Tree a b -> STArray s Edge [b] -> ST s (Tree a b)
    aux (Nd n x c) mArr = do
      let f acc (x, t) = do
            let edge = Edge (n, getNode t)
            l <- readSTArray mArr edge
            if x `elem` l
              then do
                _ <- writeSTArray mArr edge (delete x l)
                children <- aux t mArr
                return (acc ++ [(x, children)])
              else return acc

      c' <- foldM f [] c
      return (Nd n x c')
-}

prune2 :: forall a b. (Ord b, Show b) => Graph a b -> Node -> Tree a b
prune2 gr n = runST $ do
  mArr <- thawSTArray (vertexArr gr)
  aux (paths gr n) mArr
  where
    aux :: (Ord b, Show b) => forall s. Tree a b -> STArray s Node Bool -> ST s (Tree a b)
    aux (Nd n x c) mArr = do
      b <- readSTArray mArr n
      if b
        then do
          _ <- writeSTArray mArr n False
          let f acc (x, t) = do
                child <- aux t mArr
                return (acc ++ [(x, child)])
          c' <- foldM f [] c
          return (Nd n x c')
        else return (Nd n x [])

prune3 :: forall a b. (Ord b, Show b) => Graph a b -> Node -> Tree a b
prune3 gr n = runST $ do
  mArr <- thawSTArray (vertexArr gr)
  aux (paths gr n) mArr
  where
    aux :: (Ord b, Show b) => forall s. Tree a b -> STArray s Node Bool -> ST s (Tree a b)
    aux (Nd n x c) mArr = do
      b <- readSTArray mArr n
      if b
        then do
          _ <- writeSTArray mArr n False
          let f acc (x, t) = do
                child <- aux t mArr
                return (acc ++ [(x, child)])
          c' <- foldM f [] c
          return (Nd n x c')
        else return (Nd n x [])