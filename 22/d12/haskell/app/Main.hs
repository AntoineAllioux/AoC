module Main where

import Data.IntMap.Lazy as IntMap ()
import GraphRep (Gr)
import InductiveGraph

main :: IO ()
main =
  do
    handle <- readFile "input"
    let map =
          ([("g", 5)], 6, "f", [])
            & ([("e", 2)], 5, "e", [("f", 3)])
            & ([("d", 2)], 4, "d", [])
            & ([("b", 1)], 3, "c", [("c", 2)])
            & ([("a", 1), ("a", 1)], 2, "b", [])
            & ([], 1, "a", [])
            & empty ::
            Gr String String
    putStrLn (show map)
    putStrLn (show (dfs [1] map))
    putStrLn (show (bfs [1] map))
    putStrLn (show (df [1] map))

-- putStrLn (show (prune g 1))
-- putStrLn (show (prune2 g 1))

{-
putStrLn (show (graphBounds g))
putStrLn (show em)
putStrLn (show (toList em))
putStrLn (show (range (Edge (nodeMin, nodeMin), Edge (nodeMax, nodeMax))))
putStrLn (show (assocs (edgeArr g))) -}
