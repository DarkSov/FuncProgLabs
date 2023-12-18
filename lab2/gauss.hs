import Control.Parallel.Strategies

type Matrix = [[Double]]
type Vector = [Double]

parallelGauss :: Matrix -> Vector -> Vector
parallelGauss matrix vector =
  let augmentedMatrix = zipWith (++) matrix (map (:[]) vector)
      triangularMatrix = gaussElimination augmentedMatrix
  in backSubstitution triangularMatrix

gaussElimination :: Matrix -> Matrix
gaussElimination matrix = foldl1' reduceRow parallelTriangularMatrix
  where
    parallelTriangularMatrix = withStrategy (parList rdeepseq) triangularMatrix
    triangularMatrix = foldl' eliminateRows matrix [0 .. rowCount - 1]
    rowCount = length matrix

eliminateRows :: Matrix -> Int -> Matrix
eliminateRows matrix pivot =
  let (above, below) = splitAt pivot matrix
      pivotRow = head below
      parallelBelow = withStrategy (parList rdeepseq) below
      modifiedBelow = map (eliminateRow pivotRow) parallelBelow
  in above ++ modifiedBelow

reduceRow :: Matrix -> Matrix -> Matrix
reduceRow upper lower =
  let pivot = head $ head lower
      multiplier = pivot / head (head upper)
      scaledUpper = map (map (* multiplier)) upper
  in zipWith (zipWith (-)) lower scaledUpper

backSubstitution :: Matrix -> Vector
backSubstitution matrix = reverse $ foldl' substitute [last (last matrix)] (init $ init matrix)
  where
    substitute :: Vector -> Vector -> Vector
    substitute acc row =
      let lhs = init row
          rhs = last row
          coefficients = init acc
          constant = last acc
          newConstant = rhs - sum (zipWith (*) lhs coefficients)
      in newConstant : acc

main :: IO ()
main = do
  let matrix = [[2, 1, -1], [-3, -1, 2], [-2, 1, 2]]
      vector = [8, -11, -3]
      solution = parallelGauss matrix vector
  putStrLn $ "Solution: " ++ show solution
