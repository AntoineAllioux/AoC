{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Array (elems, indices, (!))
import Data.Graph
import Data.List (sortBy)
import Data.List.Split

newtype Pos = Pos (Int, Int, Int)

newtype Brick = Brick (Pos, Pos)

newtype Interval = Int (Int, Int)

getZ :: Brick -> Int
getZ (Brick (Pos (_, _, z), _)) = z

setZ :: Brick -> Int -> Brick
setZ (Brick (Pos (x11, y11, z11), Pos (x12, y12, z12))) z =
  let zLen = z12 - z11
   in Brick (Pos (x11, y11, z), Pos (x12, y12, z + zLen))

moveZ :: Brick -> Int -> Brick
moveZ b n = setZ b (getZ b + n)

getHeight :: Brick -> Int
getHeight b@(Brick (_, Pos (_, _, z))) = z - getZ b + 1

intersect :: Interval -> Interval -> Bool
intersect (Int (x11, x12)) (Int (x21, x22)) =
  x12 >= x21 && x11 <= x22

areColliding :: Brick -> Brick -> Bool
areColliding
  (Brick (Pos (x11, y11, z11), Pos (x12, y12, z12)))
  (Brick (Pos (x21, y21, z21), Pos (x22, y22, z22))) =
    intersect (Int (x11, x12)) (Int (x21, x22))
      && intersect (Int (y11, y12)) (Int (y21, y22))
      && intersect (Int (z11, z12)) (Int (z21, z22))

preds :: Graph -> Vertex -> [Vertex]
preds g v = filter (\v2 -> elem v (g ! v2)) (indices g)

bricksGraph :: [Brick] -> (Graph, _, _)
bricksGraph bricks =
  let sorted = sortBy (\b1 b2 -> compare (getZ b1) (getZ b2)) bricks
      stillB = computePos sorted []
      indexedB = zipWith (,) [1 ..] (reverse stillB)
   in graphFromEdges (edgesB indexedB)
  where
    computePos :: [Brick] -> [Brick] -> [Brick]
    computePos [] stillB = stillB
    computePos (b : bs) still =
      let z = foldl (\acc stillB -> max acc (minZ b stillB)) 1 still
       in computePos bs (setZ b z : still)

    minZ :: Brick -> Brick -> Int
    minZ b1 b2 =
      let b2Z = getZ b2
       in if areColliding b2 (setZ b1 b2Z) then b2Z + getHeight b2 else 1

    edgesB :: [(Int, Brick)] -> [(Brick, Int, [Int])]
    edgesB [] = []
    edgesB ((i, b) : bs) =
        let succ = foldl (\acc (j, b2) -> if areColliding b (moveZ b2 (-1)) then j : acc else acc) [] bs
        in (b, i, succ) : edgesB bs

part1 :: Graph -> Int
part1 graph =
  length $ filter (all (\x -> length (preds graph x) > 1)) (elems graph)

part2 :: Graph -> Int
part2 graph = foldr1 (+) (map fallCount (indices graph))
  where
    fallCount :: Vertex -> Int
    fallCount v = length (fallCountAux (graph ! v) [v]) - 1

    fallCountAux :: [Vertex] -> [Vertex] -> [Vertex]
    fallCountAux [] acc = acc
    fallCountAux (v : vs) acc =
      let succ = filter (\x -> notElem x vs) (graph ! v)
          pred = preds graph v
          (succ', acc') = if all (\x -> elem x acc) pred then (vs ++ succ, v : acc) else (vs, acc)
       in fallCountAux succ' acc'

parse :: String -> Brick
parse s =
  let [x1, y1, z1, x2, y2, z2] = map read (splitOneOf "~," s)
   in Brick (Pos (x1, y1, z1), Pos (x2, y2, z2))

main :: IO ()
main = do
  file <- readFile "input"
  let bricks = map parse (lines file)
  let (graph, _, _) = bricksGraph bricks

  putStrLn ("Part 1: " ++ show (part1 graph))
  putStrLn ("Part 2: " ++ show (part2 graph))
