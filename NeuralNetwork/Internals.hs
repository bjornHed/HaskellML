module Internals where

import Data.Array.Repa.Algorithms.Matrix
import Data.Array.Repa.Algorithms.Randomish
import Data.Array.Repa as R hiding ((++))

data NeuralNetwork = FFNN WeightsAndBiases ActivationFunction

type WeightsAndBiases = [(Array U DIM2 Double)]
-- | First is the standard activation, the second is the derivative
data ActivationFunction = Activation (Double -> Double) (Double -> Double)

instance Show NeuralNetwork where
  show (FFNN weights activation) = show weights

learningRate = 0.5
