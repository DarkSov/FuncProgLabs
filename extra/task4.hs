import Data.List

type Graph a = [(a, [a])]

allPaths :: Eq a => a -> a -> Graph a -> [[a]]
allPaths start end graph = concatMap (\neighbor -> explore [start] neighbor []) (adjacentNodes start graph)
  where
    adjacentNodes node g = case lookup node g of
      Just neighbors -> neighbors
      Nothing -> []

    explore path node visited
      | node == end = [path ++ [node]]
      | node `elem` visited = []
      | otherwise = concatMap (\neighbor -> explore (path ++ [node]) neighbor (visited ++ [node])) (adjacentNodes node graph)

main :: IO ()
main = do
  let graph = [("A", ["B", "C"]),
               ("B", ["A", "C", "D"]),
               ("C", ["A", "B", "D"]),
               ("D", ["B", "C"])]
      startNode = "A"
      endNode = "D"
  print $ allPaths startNode endNode graph
