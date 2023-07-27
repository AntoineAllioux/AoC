{-# LANGUAGE ViewPatterns #-}

-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

module InductiveGraph where

import Control.Monad (foldM)
import Control.Monad.State.Lazy
import Data.List (head, nub, null)

type Node = Int

type Adj b = [(b, Node)]

type Ctx a b = (Adj b, Node, a, Adj b)

class Graph g where
  empty :: g a b
  isEmpty :: g a b -> Bool

  -- view :: g a b -> GraphView a b g
  match :: Node -> g a b -> (Maybe (Ctx a b), g a b)
  matchAny :: g a b -> (Maybe (Ctx a b), g a b)

  infixr 0 &
  (&) :: Ctx a b -> g a b -> g a b

data GraphView a b g = Nil | Cons (Ctx a b) (g a b)

data Tree a = Lf | Nd Node a [Tree a]
  deriving (Show)

{-

isEmpty :: Graph a b -> Bool
isEmpty Nil = True
isEmpty _ = False

grev :: Graph a b -> Graph a b
grev =
	let swap (pred, n, x, suc) = (suc, n, x, pred)
	in gmap  swap
-}

-- Wrong incomplete pattern matching warnings because of view patterns
ufold :: (Graph g) => c -> (Ctx a b -> c -> c) -> g a b -> c
ufold nil cons (matchAny -> (Nothing, _)) = nil
ufold nil cons (matchAny -> (Just ctx, gr)) = cons ctx (ufold nil cons gr)

-- gmap :: Graph g => (Ctx a b -> Ctx c d) -> g a b -> g c d
-- gmap f = ufold empty (\ ctx c -> Cons (f ctx) )
{-
gmap :: Graph g => (Ctx a b -> Ctx c d) -> g a b -> g c d
gmap f (matchAny -> (Nothing, _)) = empty
gmap f (matchAny -> (Just ctx, gr)) = f ctx & (gmap f gr)
-}

gsuc :: (Graph g) => Node -> g a b -> [Node]
gsuc n (match n -> (Nothing, _)) = []
gsuc n (match n -> (Just (_, _, _, s), _)) = fmap snd s

deg :: (Graph g) => Node -> g a b -> Int
deg n (match n -> (Nothing, _)) = 0
deg n (match n -> (Just (p, _, _, s), _)) = length p + length s

del :: (Graph g) => Node -> g a b -> g a b
del n g@(match n -> (Nothing, _)) = g
del n (match n -> (Just _, gr)) = gr

suc :: Ctx a b -> [Node]
suc (_, _, _, s) = fmap snd s

{-
dfs2 :: Graph g => [Node] -> g a b -> [Node]
dfs2 [] g = []
dfs2 (n : ns) g =
    let nil' = []
        cons' (p, m, x, s) acc =
    in ufold nil' cons' g ++ dfs2 ns g
-}

dfs :: (Graph g) => [Node] -> g a b -> [Node]
dfs l g | null l || isEmpty g = []
dfs (n : ns) g@(match n -> (Nothing, _)) = dfs ns g
dfs (n : ns) g@(match n -> (Just ctx, gr)) =
  n : dfs (suc ctx ++ ns) gr

-- Has to be initially called with a single node
-- Not efficient to use lists as queues
bfs :: (Graph g) => [Node] -> g a b -> [Node]
bfs l g | null l || isEmpty g = []
bfs (n : ns) g@(match n -> (Nothing, _)) = bfs ns g
bfs (n : ns) g@(match n -> (Just ctx, gr)) =
  n : bfs (ns ++ suc ctx) gr

-- Bad performance
postorder :: Tree a -> [Node]
postorder Lf = []
postorder (Nd n _ c) = concatMap postorder c ++ [n]

preorder :: Tree a -> [Node]
preorder Lf = []
preorder (Nd n _ c) = n : concatMap preorder c

nodes :: Graph g => g a b -> [Node]
nodes (matchAny -> (Nothing, _)) = []
nodes (matchAny -> (Just (_, n, _, _), g)) = n : nodes g

df :: (Graph g) => [Node] -> g a b -> [Tree a]
df n g = fst (aux n g)
  where
    aux :: (Graph g) => [Node] -> g a b -> ([Tree a], g a b)
    aux l g | null l || isEmpty g = ([], g)
    aux (n : ns) g@(match n -> (Nothing, _)) = aux ns g
    aux (n : ns) (match n -> (Just ctx@(_, _, x, _), g)) =
      let (ns', g1) = aux (suc ctx) g
          (ts, g2) = aux ns g1
       in (Nd n x ns' : ts, g2)

topsort :: Graph g => g a b -> [Node]
topsort g = concatMap preorder (df (nodes g) g)

df2 :: Graph g => [Node] -> g a b -> [Tree a]
df2 n = evalState (aux n) 
  where 
    aux :: (Graph g) => [Node] -> State (g a b) [Tree a]
    aux [] = return []
    aux (n : ns) = do
      g <- get
      case match n g of
        (Nothing, _) -> aux ns
        (Just ctx@(_, _, x, _), g1) -> do
          _ <- put g1
          ns' <- aux (suc ctx)
          ts <- aux ns
          return ((Nd n x ns') : ts)


bf :: (Graph g) => [Node] -> g a b -> [Tree a]
bf n g = fst (aux n g)
  where
    aux :: (Graph g) => [Node] -> g a b -> ([Tree a], g a b)
    aux l g | null l || isEmpty g = ([], g)
    aux (n : ns) g@(match n -> (Nothing, _)) = aux ns g
    aux (n : ns) (match n -> (Just ctx@(_, _, x, _), g)) =
      let (ts, g1) = aux ns g
          (ns', g2) = aux (suc ctx) g1
       in (Nd n x ns' : ts, g2)

{-
nodes :: Graph a b -> [Node]
nodes = ufold [] (\ (_, n, _, _) ns -> n : ns)

undir :: Eq b => Graph a b -> Graph a b
undir =
    let f (pred, n, x, succ) gr =
        let edges = nub (pred ++ succ)
        in Cons (edges, n, x, edges) gr
    in ufold Nil f
-}
{-
ufold :: c -> (Ctx a b -> c -> c) -> Graph a b -> c
ufold nil cons Nil = nil
ufold nil cons (Cons ctx gr) = cons ctx (ufold nil cons gr)

gmap :: (Ctx a b -> Ctx c d) -> Graph a b -> Graph c d
gmap f = ufold Nil (\ ctx -> Cons (f ctx))

nodes :: Graph a b -> [Node]
nodes = ufold [] (\ (_, n, _, _) ns -> n : ns)

undir :: Eq b => Graph a b -> Graph a b
undir =
	let f (pred, n, x, succ) gr =
		let edges = nub (pred ++ succ)
		in Cons (edges, n, x, edges) gr
	in ufold Nil f

	-}
