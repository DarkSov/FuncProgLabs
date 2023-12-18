import System.Random
import Data.List

type City = (Double, Double)

data Path = Path {route :: [Int], cost :: Double}

data GAParams = GAParams
  { populationSize :: Int,
    generations :: Int,
    mutationRate :: Double
  }

randomRoute :: Int -> IO [Int]
randomRoute n = do
  gen <- newStdGen
  return $ take n $ nub $ randomRs (0, n - 1) gen

distance :: City -> City -> Double
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

calculateCost :: [City] -> [Int] -> Double
calculateCost cities routeIndices =
  sum $ zipWith distance (map (cities !!) routeIndices) (map (cities !!) (tail routeIndices ++ [head routeIndices]))

evaluatePath :: [City] -> [Int] -> Path
evaluatePath cities routeIndices = Path routeIndices (calculateCost cities routeIndices)

initializePopulation :: Int -> Int -> IO [Path]
initializePopulation popSize cityCount = do
  paths <- mapM (\_ -> randomRoute cityCount) [1 .. popSize]
  return $ map (\p -> evaluatePath cities p) paths
  where
    cities = replicate cityCount (0, 0)  

selectParents :: [Path] -> IO (Path, Path)
selectParents population = do
  parent1 <- selectParent population
  parent2 <- selectParent population
  return (parent1, parent2)

selectParent :: [Path] -> IO Path
selectParent population = do
  let totalCost = sum $ map cost population
      roulette = scanl (\acc path -> acc + cost path / totalCost) 0 population
  randomValue <- randomRIO (0, 1)
  let selectedPath = head $ filter (\(_, acc) -> acc >= randomValue) $ zip population roulette
  return $ fst selectedPath

crossover :: Path -> Path -> IO Path
crossover (Path route1 _) (Path route2 _) = do
  crossoverPoint <- randomRIO (1, length route1 - 1)
  let childRoute = take crossoverPoint route1 ++ filter (`notElem` take crossoverPoint route1) route2
  return $ evaluatePath cities childRoute
  where
    cities = replicate (length route1) (0, 0)

mutate :: Path -> Double -> IO Path
mutate (Path route cost) mutationRate = do
  shouldMutate <- randomRIO (0, 1) :: IO Double
  if shouldMutate < mutationRate
    then do
      index1 <- randomRIO (0, length route - 1)
      index2 <- randomRIO (0, length route - 1)
      let mutatedRoute = swapElements index1 index2 route
      return $ evaluatePath cities mutatedRoute
    else return $ Path route cost
  where
    cities = replicate (length route) (0, 0)
    swapElements i j xs = let elemI = xs !! i in take i xs ++ [xs !! j] ++ drop (i + 1) (take j xs) ++ [elemI] ++ drop (j + 1) xs

geneticAlgorithm :: GAParams -> [City] -> IO Path
geneticAlgorithm params cities = do
  initialPopulation <- initializePopulation (populationSize params) (length cities)
  finalPopulation <- evolve params initialPopulation
  return $ minimumBy (\p1 p2 -> compare (cost p1) (cost p2)) finalPopulation

evolve :: GAParams -> [Path] -> IO [Path]
evolve params population = evolve' params (generations params) population
  where
    evolve' params 0 population' = return population'
    evolve' params gen population' = do
      newPopulation <- generateNewPopulation params population'
      evolve' params (gen - 1) newPopulation

generateNewPopulation :: GAParams -> [Path] -> IO [Path]
generateNewPopulation params population = do
  children <- replicateM (populationSize params `div` 2) $ do
    (parent1, parent2) <- selectParents population
    child1 <- crossover parent1 parent2
    child2 <- crossover parent2 parent1
    mutatedChild1 <- mutate child1 (mutationRate params)
    mutatedChild2 <- mutate child2 (mutationRate params)
    return [mutatedChild1, mutatedChild2]
  return $ take (populationSize params) $ population ++ concat children

main :: IO ()
main = do
  let cityCount = 10
      gaParams = GAParams
        { populationSize = 100,
          generations = 1000,
          mutationRate = 0.1
        }
  cities <- replicateM cityCount $ do
    x <- randomRIO (0, 100) :: IO Double
    y <- randomRIO (0, 100) :: IO Double
    return (x, y)
  result <- geneticAlgorithm gaParams cities
  putStrLn $ "Optimal path: " ++ show (route result)
  putStrLn $ "Optimal cost: " ++ show (cost result)
