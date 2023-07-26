{-# LANGUAGE ViewPatterns #-}

module GraphRep where

import Data.IntMap.Lazy
  ( IntMap,
    delete,
    findMin,
    foldlWithKey,
    insert,
    insertWith,
    mapWithKey,
    member,
    (!),
    (!?),
  )
import qualified Data.IntMap.Lazy as IntMap
import InductiveGraph

type Ctx' a b = (IntMap [b], a, IntMap [b])

newtype Gr a b = Gr {gr :: IntMap (Ctx' a b)}
  deriving (Show)

labEdges :: IntMap [b] -> [(b, Int)]
labEdges = foldlWithKey (\acc k v -> fmap (\x -> (x, k)) v ++ acc) []

delNode :: Node -> Gr a b -> Gr a b
delNode n g@(Gr gr) =
  let f (p, x, s) = (delete n p, x, delete n s)
   in Gr (fmap f (delete n gr))

{-
  | False <- member n gr = g
  | _ <- member n gr =
      let f (p, x, s) = (delete n p, x, delete n s)
       in Gr (fmap f (delete n gr))
-}
-- Be careful
addNode :: Ctx a b -> Gr a b -> Gr a b
addNode (p, n, x, s) (Gr gr) =
  let g acc (x, m) = insertWith (++) m [x] acc
      p' = foldl g IntMap.empty p
      s' = foldl g IntMap.empty s

      f m (p, x, s) =
        let p'' = maybe p (\x -> insert n x p) (s' !? m)
            s'' = maybe s (\x -> insert n x s) (p' !? m)
         in (p'', x, s'')
   in Gr (insert n (p', x, s') (mapWithKey f gr))

instance Graph Gr where
  empty = Gr IntMap.empty

  isEmpty (Gr gr) = IntMap.null gr

  match n g@(Gr gr)
    | Nothing <- gr !? n = (Nothing, g)
    | Just (p, x, s) <- gr !? n =
        let p' = labEdges p
            s' = labEdges s
            g' = delNode n g
         in (Just (p', n, x, s'), g')

  matchAny g@(Gr gr) = match (fst (findMin gr)) g

  (&) = addNode
