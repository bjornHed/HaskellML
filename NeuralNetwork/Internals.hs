module Internals where

import Data.Array.Repa.Algorithms.Matrix
import Data.Array.Repa.Algorithms.Randomish
import Data.Array.Repa as R hiding ((++))

data NeuralNetwork = FFNN WeightsAndBiases ActivationFunction

type WeightsAndBiases = [(Array U DIM2 Double)]
-- | First is the standard activation, the second is the derivative
data ActivationFunction = Activation (Double -> Double) (Double -> Double)

data TrainingSettings = TS {
  epochs :: Int, -- ^ Maximum number of epochs run
  earlyStopping :: Bool, -- ^ Enable early stopping
  validationSplit :: Double, -- ^ Percentage of data as validation
  verbose :: Bool, -- ^ Toggle for printing progress
  batchSize :: Int
}

-- | A layer for a Feed-forward neural network
data Layer = Layer {
  weights    :: Array U DIM2 Double,
  bias       :: Array U DIM2 Double,
  activation :: ActivationFunction
}

defaultTrainingSettings :: TrainingSettings
defaultTrainingSettings = TS 100 True 0.8 True 34

-- | List Input and target lists
type TrainingData = [([Double],[Double])]

instance Show NeuralNetwork where
  show (FFNN weights activation) = show weights

learningRate = 0.5
