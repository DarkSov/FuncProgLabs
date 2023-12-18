import Data.List (permutations)

type Position = (Int, Int)

isValidMove :: Int -> Position -> Position -> Bool
isValidMove n (x, y) (newX, newY) =
  newX >= 1 && newX <= n && newY >= 1 && newY <= n

knightMoves :: Int -> Position -> [Position]
knightMoves n (x, y) =
  filter (isValidMove n (x, y)) $
    [ (x + 2, y + 1),
      (x + 1, y + 2),
      (x - 1, y + 2),
      (x - 2, y + 1),
      (x - 2, y - 1),
      (x - 1, y - 2),
      (x + 1, y - 2),
      (x + 2, y - 1)
    ]

knightTour :: Int -> Position -> [[Position]]
knightTour n start =
  filter (\path -> length path == n * n) $
    map (start :) (go [start])
  where
    go path@(current : _) =
      let possibleMoves = filter (`notElem` path) (knightMoves n current)
       in case possibleMoves of
            [] -> [path]
            _ -> concatMap (\move -> go (move : path)) possibleMoves

main :: IO ()
main = do
  let n = 5 -- розмір шахівниці
      start = (1, 1) -- початкова позиція коня
  print $ knightTour n start
