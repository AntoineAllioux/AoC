{-# LANGUAGE ViewPatterns #-}

-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

module InductiveGraph where

import Data.List (head, nub, null)
import System.Posix.Internals (newFilePath)

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

data Tree a b = Lf | Nd Node a [(b, Tree a b)]
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
postorder :: Tree a b -> [a]
postorder Lf = []
postorder (Nd _ x c) = concatMap (postorder . snd) c ++ [x]

df :: (Graph g) => [Node] -> g a b -> ([Tree a b], g a b)
df l g | null l || isEmpty g = ([], g)
df (n : ns) g@(match n -> (Nothing, _)) = df ns g
df (n : ns) g@(match n -> (Just ctx@(p, _, x, s), gr)) =
  let -- (tr, g2) = df (suc ctx ++ ns) gr
      (yo, gr2) =
        foldl
          ( \(l, grr) (b, m) ->
              let yoo = df [m] grr
               in (l ++ fmap (\x -> (b, x)) (fst yoo), snd yoo)
          )
          ([], gr)
          s
      (ns', gr3) = df ns gr2
   in ((Nd n x yo) : ns', gr3)

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
