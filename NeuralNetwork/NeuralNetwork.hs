{-# LANGUAGE FlexibleContexts #-}

module NeuralNetwork (
  NeuralNetwork,
  sigmoid,
  feedForwardS,
  feedForwardP,
  initiateNetwork,
  trainOne,
  toVector) where

import Data.Array.Repa.Algorithms.Matrix
import Data.Array.Repa.Algorithms.Randomish
import Data.Array.Repa as R hiding ((++))
import Data.Foldable

-- | -------------------------------
-- | Data and types
-- | -------------------------------
data NeuralNetwork = FFNN WeightsAndBiases ActivationFunction

type WeightsAndBiases = [(Array U DIM2 Double)]
-- | First is the standard activation, the second is the derivative
data ActivationFunction = Activation (Double -> Double) (Double -> Double)

instance Show NeuralNetwork where
  show (FFNN weights activation) = show weights

learningRate = 0.5

sigmoid :: ActivationFunction
sigmoid = Activation (\x -> 1.0 / (1.0 + exp( -x ))) (\x -> x * (1.0 - x))

-- | Helpers
layers :: NeuralNetwork -> Int
layers (FFNN network _) = (length network) `div` 2

-- | Train a network by backpropagation
trainOne :: [Double] -> Array U DIM2 Double -> NeuralNetwork -> IO NeuralNetwork
trainOne input target network@(FFNN weights act@(Activation f df)) = do
    (delta, newWeights) <- nextLayer (toVector input) 0
    return $ FFNN newWeights act
  where addBias input weightIndex = R.zipWith (+) input (weights !! (2*weightIndex+1))
        applyActivation input = R.map f input
        bias index = weights !! (2*index+1)
        nextLayer input index
          | index == (layers network)-1 = do
              initialOutput <- ((weights !! (index*2)) `mmultP` input)
              let output = applyActivation $ addBias initialOutput index
              --print $ computeUnboxedS output
              let dOdN   = R.map df output
              let err = R.zipWith (\x -> \y -> -(x-y)) target output
              delta <- computeUnboxedP $ R.zipWith (*) dOdN err
              h <- computeUnboxedP $ transpose input
              weightDiff <- delta `mmultP` h
              let update = R.map (\x -> learningRate*x) weightDiff
              newWeights <- computeUnboxedP $ R.zipWith (-) (weights !! (2*index)) $ update
              return (delta,[newWeights, bias index])
          | otherwise = do
              initialOutput <- ((weights !! (index*2)) `mmultP` input)
              output <- computeUnboxedP $ applyActivation $ addBias initialOutput index
              (delta, constructedWeights) <- nextLayer output (index + 1)
              dOdN <- computeUnboxedP $ R.map df output
              transp <- computeUnboxedP $ transpose delta
              multiplier <- transp `mmultP`(weights !! (index + 2))
              newDelta <- dOdN `mmultP` multiplier
              h <- computeUnboxedP $ transpose input
              weightDiff <- newDelta `mmultP` h
              let update = R.map (\x -> learningRate*x) weightDiff
              newWeights <- computeUnboxedP $ R.zipWith (-) (weights !! (2*index)) $ update
              return (newDelta,[newWeights, bias index] ++ constructedWeights)

toVector :: [Double] -> Array U DIM2 Double
toVector input = fromListUnboxed (Z :. (length input) :. (1 :: Int)) input

-- | Essentials
feedForwardS :: [Double] -> NeuralNetwork -> (Array U DIM2 Double)
feedForwardS input network@(FFNN weights (Activation activation _ )) =
    foldr calculateOneStep (toVector input) layerIndices
  where layerIndices = reverse [0..(layers network)-1]
        calculateOneStep layerIndex currentInput =
          applyActivation $ addBias ((weights !! (layerIndex*2)) `mmultS` currentInput) layerIndex
        addBias input weightIndex = R.zipWith (+) input (weights !! (2*weightIndex+1))
        applyActivation input = computeUnboxedS $ R.map activation input

feedForwardP :: [Double] -> NeuralNetwork -> IO (Array U DIM2 Double)
feedForwardP input network@(FFNN weights (Activation activation _ )) = do
    foldrM calculateOneStep (toVector input) layerIndices
  where layerIndices = reverse [0..(layers network)-1]
        calculateOneStep layerIndex currentInput = do
          initialOutput <- ((weights !! (layerIndex*2)) `mmultP` currentInput)
          applyActivation $ addBias initialOutput layerIndex
        addBias input weightIndex = R.zipWith (+) input (weights !! (2*weightIndex+1))
        applyActivation input = computeUnboxedP $ R.map activation input

networkError :: [Double] -> [Double] -> NeuralNetwork -> IO Double
networkError input target network = do
  output <- feedForwardP input network
  let out = R.toList output
  return $ sum $ Prelude.map (\x -> (x*x)/2) $ Prelude.zipWith (-) target out

initiateNetwork :: [Int]-> ActivationFunction -> Int -> NeuralNetwork
initiateNetwork nodesInLayers activation seed = FFNN generateWeights activation
  where generateWeights = foldr generateLayer [] [0..length nodesInLayers-2]
        generateLayer index layers =
          layers ++ [(generateEdges index),(generateBias index)]
        generateEdges index =
          let x = nodesInLayers !! index in
          let y = (nodesInLayers ++ [1]) !! (index + 1) in
          (randomishDoubleArray (Z :. x :. y) (-1.0) 1.0 (seed*19*(index+1)))
        generateBias index  =
          let x = nodesInLayers !! index in
          (randomishDoubleArray (Z :. x :. (1 :: Int)) (-1.0) 1.0 (seed*47*(index+1)))
