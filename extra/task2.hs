import Numeric.LinearAlgebra

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Double -> Double
sigmoid' x = sigmoid x * (1 - sigmoid x)

data NeuralNetwork = NeuralNetwork
  { weights1 :: Matrix Double,
    biases1 :: Vector Double,
    weights2 :: Matrix Double,
    biases2 :: Vector Double
  }

initializeNetwork :: Int -> Int -> Int -> NeuralNetwork
initializeNetwork inputSize hiddenSize outputSize =
  NeuralNetwork
    { weights1 = randn hiddenSize inputSize,
      biases1 = randn hiddenSize 1,
      weights2 = randn outputSize hiddenSize,
      biases2 = randn outputSize 1
    }

feedforward :: NeuralNetwork -> Vector Double -> Vector Double
feedforward nn input =
  let layer1 = sigmoid $ (weights1 nn) #> input + (biases1 nn)
      layer2 = sigmoid $ (weights2 nn) #> layer1 + (biases2 nn)
   in layer2

train :: NeuralNetwork -> Matrix Double -> Matrix Double -> Double -> NeuralNetwork
train nn inputs targets learningRate =
  let (outputs1, outputs2) = forwardPass nn inputs
      outputError = targets - outputs2
      deltaOutput = outputError *^ (outputs2 * (1 - outputs2))
      hiddenLayerError = tr (weights2 nn) #> deltaOutput
      deltaHidden = hiddenLayerError *^ (outputs1 * (1 - outputs1))
      weights2' = weights2 nn + scalar learningRate `outer` deltaOutput `outer` outputs1
      biases2' = biases2 nn + scalar learningRate * deltaOutput
      weights1' = weights1 nn + scalar learningRate `outer` deltaHidden `outer` inputs
      biases1' = biases1 nn + scalar learningRate * deltaHidden
   in nn {weights1 = weights1', biases1 = biases1', weights2 = weights2', biases2 = biases2'}

forwardPass :: NeuralNetwork -> Matrix Double -> (Matrix Double, Matrix Double)
forwardPass nn inputs =
  let layer1 = sigmoid $ (weights1 nn) #> inputs + (repmat (biases1 nn) 1 (cols inputs))
      layer2 = sigmoid $ (weights2 nn) #> layer1 + (repmat (biases2 nn) 1 (cols layer1))
   in (layer1, layer2)

main :: IO ()
main = do
  let inputSize = 2
      hiddenSize = 4
      outputSize = 1
      learningRate = 0.1
      epochs = 10000
      inputs = (3 >< 2) [0, 0, 0, 1, 1, 0, 1, 1]
      targets = (3 >< 1) [0, 1, 1]
      initialNetwork = initializeNetwork inputSize hiddenSize outputSize
      trainedNetwork = trainNetwork epochs initialNetwork inputs targets learningRate
  print $ "Trained Network Output: " ++ show (forwardPass trainedNetwork inputs)
