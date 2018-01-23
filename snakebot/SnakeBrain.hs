module SnakeBrain where

import Data.Matrix as M;
import Control.Monad.Random as R;
import SnakeUtils

brainFromChromosome :: [Float] -> Int -> Int -> Int -> SnakeNet
brainFromChromosome chromosome input 0 output =
  let weights = M.fromList input output $ take (input*output) chromosome in
  let bias    = take output $ drop (input*output) chromosome in
  SnakeNet [weights] [bias]
brainFromChromosome chromosome input hidden output =
  let inpToHid = M.fromList input hidden $ take (input*hidden) chromosome in
  let hidToOut = M.fromList hidden output $ take (hidden*output) $ drop (input*hidden) chromosome in
  let hidBias  = take hidden $ drop ((input*hidden) + (hidden*output)) chromosome in
  let outBias  = take output $ drop ((input*hidden) + (hidden*output) + hidden) chromosome in
  SnakeNet [inpToHid,hidToOut] [hidBias,outBias]

calculate :: Matrix Float -> SnakeNet -> IO Direction
calculate input network
  | (length input) /= (nrows ((weights network) !! 0)) = do
      print $ length input
      print $ (nrows ((weights network) !! 0))
      error "Input does not match network"
  | otherwise =
      let hiddenOutput =
            mapRow (\_ x -> sigmoid x) 1 $ addBias (head (bias network)) $ M.multStd input (head (weights network)) in
      let networkOutput =
            mapRow (\_ x -> outputActivation x) 1 $ addBias (last (bias network)) $ M.multStd hiddenOutput (last (weights network)) in
      return $ interpretDirection(M.toList networkOutput)

-- | Matrix must be a single row
addBias :: [Float] -> Matrix Float -> Matrix Float
addBias bias result = biasAdder bias result $ (length bias) - 1
  where biasAdder bias res 0 = res
        biasAdder bias res i =
          biasAdder bias (setElem ((res ! (1,i)) + (bias !! i)) (1,i) res) (i-1)

outputActivation :: Float -> Float
outputActivation x =
  if x < 0.0
  then 0.0
  else 1.0

sigmoid :: Float -> Float
sigmoid x = 1/(1 + (exp (-x)))

interpretDirection :: [Float] -> Direction
interpretDirection (x:y:_)
  | x == 0.0 && y == 0.0 = LEFT
  | x == 1.0 && y == 1.0 = RIGHT
  | otherwise = AHEAD
