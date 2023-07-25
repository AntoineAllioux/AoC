{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (foldM)
import Control.Monad.ST.Lazy
import Control.Monad.State.Lazy
import Data.IntMap.Lazy (IntMap, foldlWithKey, fromList, keys, (!))
import Data.Ix
import Data.List (delete)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import GHC.Arr (Array (Array), newSTArray, thawSTArray, readSTArray, writeSTArray, array)
import System.IO ()

type Node = Int

type Graph a b = IntMap (a, [(b, Node)])

data Tree a b = Nd Node a [(b, Tree a b)]
  deriving (Show)

newtype Edge = Edge (Node, Node)
  deriving (Eq, Ord, Ix)

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

bounds :: Graph a b -> (Node, Node)
bounds gr =
  let k = keys gr
   in (minimum k, maximum k)

paths :: Graph a b -> Node -> Tree a b
paths gr n =
  let (x, nodes) = gr ! n
      children = fmap (\(b, n) -> (b, paths gr n)) nodes
   in Nd n x children

edgeMap :: (Ord b) => Graph a b -> EdgeMap b
edgeMap gr =
  let g m acc (x, n) = Map.insertWith (\_ l -> x : l) (Edge (m, n)) [x] acc
      f acc m (_, edges) = foldl (g m) acc edges
   in foldlWithKey f Map.empty gr

getNode :: Tree a b -> Node
getNode (Nd n _ _) = n

prune :: forall a b. (Show b, Eq b) => Tree a b -> EdgeMap b -> Tree a b
prune tr em = evalState (aux tr) em
  where
    aux :: (Show b, Eq b) => Tree a b -> State (EdgeMap b) (Tree a b)
    aux (Nd n x c) = do
      let f acc (x, t) = do
            em <- get
            let edge = Edge (n, getNode t)

            if x `elem` (Map.!) em edge
              then do
                _ <- put (Map.update (\i -> Just (delete x i)) edge em)
                children <- aux t
                return (acc ++ [(x, children)])
              else return acc

      c' <- foldM f [] c
      return (Nd n x c')

edgeArr :: (Ord b) => Graph a b -> Array Edge [b]
edgeArr gr =
  let g m acc (x, n) =   --Map.insertWith (\_ l -> x : l) (Edge (m, n)) [x] acc
      f acc m (_, edges) = foldl (g m) acc edges
   in foldlWithKey f (array (bounds gr) []) gr

prune' :: (Ord b, Show b) => Graph a b -> Node -> Tree a b
prune' gr n =
  let tr = paths gr n
       
  in runST (aux tr)
  where
    -- aux :: (Ord b, Show b) => Tree a b -> ST (Array (Edge b) Int) (Tree a b)
    aux :: (Ord b, Show b) => forall s. Tree a b -> ST s (Tree a b)
    aux (Nd n x c) = do
      let f acc (x, t) = do
            let arr = edgeArr gr
            mArr <- thawSTArray arr
            let edge = Edge (n, getNode t)
            l <- readSTArray mArr edge
            if x `elem` l
              then do
                _ <- writeSTArray mArr edge (delete x l)
                children <- aux t
                return (acc ++ [(x, children)])
              else return acc

      c' <- List.foldM f [] c
      return (Nd n x c')

main :: IO ()
main =
  do
    handle <- readFile "input"

    let em = edgeMap g
    putStrLn (show (prune (paths g 1) em))
