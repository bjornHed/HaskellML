{-# LANGUAGE FlexibleContexts #-}

module NeuralNetwork (
  sigmoid,
  rectifier,
  feedForwardS,
  feedForwardP,
  initiateNetwork,
  trainNetwork) where

import Internals
import Data.Foldable
import Data.Array.Repa.Algorithms.Matrix
import Data.Array.Repa.Algorithms.Randomish
import Data.Array.Repa as R hiding ((++))
import System.Random
import Data.Array.IO
import Control.Monad
import System.ProgressBar

sigmoid :: ActivationFunction
sigmoid = Activation (\x -> 1.0 / (1.0 + exp( -x ))) (\x -> x * (1.0 - x))

rectifier :: ActivationFunction
rectifier = Activation (\x -> log(1.0 + exp(x))) (\x -> 1.0 / (1.0 + exp( -x )))

-- | Helpers
layers :: NeuralNetwork -> Int
layers (FFNN network _) = (length network) `div` 2

trainNetwork :: NeuralNetwork -> TrainingData -> TrainingSettings -> IO NeuralNetwork
trainNetwork network dataset (TS epochs True split True batch) = do
  let dat = transform dataset
  progref <- initProg epochs
  foldrM (\dat -> \net -> runEpochV dat net progref) network $ replicate epochs dat
trainNetwork network dataset (TS epochs False _ True batch) = do
  let dat = transform dataset
  progref <- initProg epochs
  foldrM (\dat -> \net -> runEpochV dat net progref) network $ replicate epochs dat
trainNetwork network dataset (TS epochs True split False batch) = do
  let dat = transform dataset
  foldrM runEpoch network $ replicate epochs dat
trainNetwork network dataset (TS epochs False _ False batch) = do
  let dat = transform dataset
  foldrM runEpoch network $ replicate epochs dat

-- | Train helpers
initProg epochs = do
  let prog = Progress 0 (toInteger (epochs+1))
  (progref, _) <- startProgress (msg "Training") (msg "") 20 prog
  return progref

runEpochV dataset network progref = do
  trainingdata <- shuffle dataset
  incProgress progref (1 :: Integer)
  foldrM (\(a,b) -> \net -> trainOne a b net) network trainingdata
runEpoch dataset network = do
  trainingdata <- shuffle dataset
  foldrM (\(a,b) -> \net -> trainOne a b net) network trainingdata
transform dataset =
  Prelude.map (\(a,b) -> (toVector a, toVector b)) dataset

-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
      where
        n = length xs
        newArray :: Int -> [a] -> IO (IOArray Int a)
        newArray n xs =  newListArray (1,n) xs

-- | Train a network by backpropagation
trainOne :: Array U DIM2 Double -> Array U DIM2 Double -> NeuralNetwork -> IO NeuralNetwork
trainOne input target network@(FFNN weights act@(Activation f df)) = do
    (_, newWeights) <- nextLayer input 0
    return $ FFNN newWeights act
  where addBias input weightIndex = R.zipWith (+) input (weights !! (2*weightIndex+1))
        {-# INLINE applyActivation #-}
        applyActivation input = R.map f input
        {-# INLINE softmax #-}
        softmax xs =
          let total = sum (R.toList xs) in
          R.map (\x -> x/total) xs
        bias index = weights !! (2*index+1)
        {-# INLINE nextLayer #-}
        nextLayer inp index
          | index == (layers network)-1 = do
              transp <- computeUnboxedP $ R.transpose $ (weights !! (index*2))
              initialOutput <- (transp `mmultP` inp)
              let output = applyActivation $ addBias initialOutput index
              let dOdN   = R.map df output
              let err = R.zipWith (\x -> \y -> -(x-y)) target output
              delta <- computeUnboxedP $ R.zipWith (*) dOdN err
              h <- computeUnboxedP $ transpose inp
              weightDiff <- delta `mmultP` h
              let update = R.transpose $ R.map (\x -> learningRate*x) weightDiff
              newWeights <- computeUnboxedP $ R.zipWith (-) (weights !! (2*index)) $ update
              return (delta,[newWeights, bias index])
          | otherwise = do
              transp <- computeUnboxedP $ R.transpose $ (weights !! (index*2))
              initialOutput <- (transp `mmultP` inp)
              output <- computeUnboxedP $ applyActivation $ addBias initialOutput index
              (delta, constructedWeights) <- nextLayer output (index + 1)
              dOdN <- computeUnboxedP $ R.map df output
              transp <- computeUnboxedP $ transpose delta
              multiplier <- transp `mmultP`(weights !! (index + 2))
              newDelta <- dOdN `mmultP` multiplier
              h <- computeUnboxedP $ transpose inp
              weightDiff <- newDelta `mmultP` h
              let update = R.map (\x -> learningRate*x) weightDiff
              newWeights <- computeUnboxedP $ R.zipWith (-) (weights !! (2*index)) $ update
              return (newDelta,[newWeights, bias index]++constructedWeights)

toVector :: [Double] -> Array U DIM2 Double
toVector input = fromListUnboxed (Z :. (length input) :. (1 :: Int)) input

-- | Essentials
{-# INLINE feedForwardS #-}
feedForwardS :: [Double] -> NeuralNetwork -> (Array U DIM2 Double)
feedForwardS input network@(FFNN weights (Activation activation _ )) =
    foldr calculateOneStep (toVector input) layerIndices
  where layerIndices = [0..(layers network)-1]
        calculateOneStep layerIndex currentInput =
          applyActivation $ addBias ((weights !! (layerIndex*2)) `mmultS` currentInput) layerIndex
        addBias input weightIndex = R.zipWith (+) input (weights !! (2*weightIndex+1))
        applyActivation input = computeUnboxedS $ R.map activation input

{-# INLINE feedForwardP #-}
feedForwardP :: [Double] -> NeuralNetwork -> IO (Array U DIM2 Double)
feedForwardP input network@(FFNN weights (Activation activation _ )) = do
    foldrM calculateOneStep (toVector input) layerIndices
  where layerIndices = reverse [0..(layers network)-1]
        {-# INLINE calculateOneStep #-}
        calculateOneStep layerIndex currentInput = do
          h <- computeUnboxedP $ R.transpose $ (weights !! (layerIndex*2))
          initialOutput <- h `mmultP` currentInput
          applyActivation $ addBias initialOutput layerIndex
        {-# INLINE addBias #-}
        addBias input weightIndex = R.zipWith (+) input (weights !! (2*weightIndex+1))
        {-# INLINE applyActivation #-}
        applyActivation input = computeUnboxedP $ R.map activation input

-- | Calculates the error if the network for one input
networkError :: [Double] -> [Double] -> NeuralNetwork -> IO Double
networkError input target network = do
  output <- feedForwardP input network
  let out = R.toList output
  return $ sum $ Prelude.map (\x -> (x*x)/2) $ Prelude.zipWith (-) target out

initiateNetwork :: [Int]-> ActivationFunction -> Int -> NeuralNetwork
initiateNetwork nodesInLayers activation seed = FFNN generateWeights activation
  where generateWeights = foldr generateLayer [] [0..length nodesInLayers-2]
        generateLayer index layers =
          [(generateEdges index),(generateBias index)] ++ layers
        generateEdges index =
          let x = nodesInLayers !! index in
          let y = nodesInLayers !! (index + 1) in
          (randomishDoubleArray (Z :. x :. y) (-1.0) 1.0 (seed*19*(index+1)))
        generateBias index  =
          let x = nodesInLayers !! index in
          (randomishDoubleArray (Z :. x :. (1 :: Int)) (-1.0) 1.0 (seed*47*(index+1)))
