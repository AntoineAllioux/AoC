{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Monad as List
import Control.Monad.State.Lazy
import Data.IntMap.Lazy (IntMap, foldlWithKey, fromList, (!))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import System.IO ()

type Node = Int

type Graph a b = IntMap (a, [(b, Node)])

data Tree a b = Nd Node a [(b, Tree a b)]
  deriving (Show)

type Edge b = (Node, Node, b)

type EdgeMap b = Map (Edge b) Int

g :: Graph () String
g =
  fromList
    [ (1, ((), [("a", 2), ("a", 2), ("b", 3)])),
      (2, ((), [("d", 4), ("e", 5)])),
      (3, ((), [("c", 2)])),
      (4, ((), [])),
      (5, ((), [("f", 3)]))
    ]

paths :: Graph a b -> Node -> Tree a b
paths gr n =
  let (x, nodes) = gr ! n
      children = fmap (\(b, n) -> (b, paths gr n)) nodes
   in Nd n x children

edgeMap :: (Ord b) => Graph a b -> EdgeMap b
edgeMap gr =
  let g m acc (x, n) = Map.insertWith (\_ i -> i + 1) (m, n, x) 1 acc
      f acc m (_, edges) = foldl (g m) acc edges
   in foldlWithKey f Map.empty gr

getNode :: Tree a b -> Node
getNode (Nd n _ _) = n

prune :: (Ord b, Show b) => Tree a b -> EdgeMap b -> Tree a b
prune tr em = evalState (aux tr) em
  where
    aux :: (Ord b, Show b) => Tree a b -> State (EdgeMap b) (Tree a b)
    aux (Nd n x c) = do
      let f acc (x, t) = do
            em <- get
            let edge = (n, getNode t, x)

            if (Map.!) em edge > 0
              then do
                _ <- put (Map.update (\i -> Just (i - 1)) edge em)
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
