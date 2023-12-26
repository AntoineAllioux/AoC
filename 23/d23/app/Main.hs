module Main where

import Data.Array (Array, bounds, listArray, (!))
import Data.Graph (Graph, buildG, graphFromEdges, scc)
import Data.Ix
import Data.Map (Map, foldrWithKey, keys, mapWithKey)
import qualified Data.Map as Map
import Data.Maybe
import Data.Sequence (Seq (Empty, (:<|)), fromList, singleton, (><))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (flatten)

type Grid = Array Int (Array Int Char)

newtype Pos = Pos (Int, Int) deriving (Eq, Ord)

type Vertex = Int

neighbours :: Grid -> Pos -> [Pos]
neighbours grid (Pos (x, y)) = aux (grid ! y ! x)
  where
    aux :: Char -> [Pos]
    aux '>' = [Pos (x + 1, y)]
    aux '<' = [Pos (x - 1, y)]
    aux '^' = [Pos (x, y - 1)]
    aux 'v' = [Pos (x, y + 1)]
    aux _ = filter (\p -> inBoundaries p && isPath p) positions

    inBoundaries (Pos (x, y)) =
      inRange (bounds (grid ! 1)) x
        && inRange (bounds grid) y

    positions = [Pos (x + 1, y), Pos (x - 1, y), Pos (x, y - 1), Pos (x, y + 1)]

    isPath (Pos (x, y)) = grid ! y ! x /= '#'

searchLongestDistance :: Graph -> Map Vertex Int -> Vertex -> Vertex -> Maybe Int
searchLongestDistance graph weights src tgt =
  let queue = singleton (src, Set.insert src Set.empty)
    in loop queue Nothing
  where
    loop :: Seq (Vertex, Set Vertex) -> Maybe Int -> Maybe Int
    loop Empty acc = acc
    loop ((n, visited) :<| ns) acc =
      let succ = graph ! n
          next = fromList (fmap (\x -> (x, Set.insert x visited)) (filter (\x -> notElem x visited) succ))
          acc' =
            if n == tgt
              then
                let dist = foldl (\acc i -> acc + (Map.!) weights i) (-1) visited
                  in foldl (\_ v -> Just (max v dist)) (Just dist) acc
              else acc
        in loop (next >< ns) acc'

longestDistance :: Grid -> Pos -> Pos -> Bool -> Maybe Int
longestDistance grid src tgt isSymmetric = do
  srcV <- vertexFromKey src
  tgtV <- vertexFromKey tgt
  srcIdx <- foldrWithKey (\i c acc -> if srcV `elem` c then Just i else acc) Nothing components
  tgtIdx <- foldrWithKey (\i c acc -> if tgtV `elem` c then Just i else acc) Nothing components
  searchLongestDistance componentsGraph weights srcIdx tgtIdx
  where
    (yMin, yMax) = bounds grid
    (xMin, xMax) = bounds (grid ! yMin)
    ranges = [(x, y) | x <- [xMin .. xMax], y <- [yMin .. yMax]]
    nodes = filter (\(x, y) -> grid ! y ! x /= '#') ranges
    edges = map (\(x, y) -> (Pos (x, y), Pos (x, y), neighbours grid (Pos (x, y)))) nodes
    (graph, _, vertexFromKey) = graphFromEdges edges

    areConnected i j =
      let v1 = (Map.!) components i
          v2 = (Map.!) components j
          f vs v = any (\v' -> v' `elem` vs) (graph ! v)
       in any (f v2) v1 || if isSymmetric then any (f v1) v2 else False

    weights =
      let computeWeights i vs =
            let connexionsCount = length [v | v <- vs, v' <- graph ! v, (j, c') <- Map.toList components, j /= i, v' `elem` c']
             in length vs - max 0 (connexionsCount - 1)
       in mapWithKey computeWeights components

    components = Map.fromList $ zip [0 ..] (map flatten (scc graph))

    componentsGraph =
      buildG
        (0, length components - 1)
        [(i, j) | i <- keys components, j <- keys components, i /= j, areConnected i j]

main :: IO ()
main = do
  input <- readFile "input"
  let gridL = lines input
  let height = length gridL
  let width = length (head gridL)
  let (src, tgt) = (Pos (2, 1), (Pos (width - 1, height)))
  let grid = listArray (1, height) (map (listArray (1, width)) gridL)
  putStrLn ("Part 1: " ++ show (fromJust (longestDistance grid src tgt False)))
  putStrLn ("Part 2: " ++ show (fromJust (longestDistance grid src tgt True)))
