module ParticleSwarm where

import SnakeUtils
import SnakeGame
import SnakeBrain
import Control.Monad.Random as R;
import Data.Maybe

data SnakeParticle = SnakeParticle {
  position  :: [Float],
  velocity  :: [Float],
  bestValue :: [Float],
  bVal :: Float
} deriving ( Show )

data SnakeSwarm = SnakeSwarm {
  population :: [SnakeParticle],
  swarmBest  :: [Float]
} deriving ( Show )

test :: IO ()
test = do
  net <- particleSwarmOptimization
  val <- evaluateSnake net
  print val

particleSwarmOptimization :: IO SnakeNet
particleSwarmOptimization = do
  swarm <- initializeSwarm populationSize
  particleSwarm swarm weightInertia iterations

particleSwarm :: SnakeSwarm -> Float -> Int -> IO SnakeNet
particleSwarm (SnakeSwarm pop currentBest) _ 0 =
  return $ toSnakeNet currentBest
particleSwarm (SnakeSwarm pop currentBest) inertiaWeight iteration = do
  print (iterations - iteration + 1)
  fitness <- mapM (\s -> evaluateSnake (toSnakeNet (position s))) pop
  currentVal <- evaluateSnake (toSnakeNet currentBest)
  let indexLookup = zip fitness $ map position pop
  let sBest = (if (maximum fitness) > currentVal then (fromJust (lookup (maximum fitness) indexLookup)) else currentBest)
  newPopulation <- sequence [updateParticleBest (pop !! i) (fitness !! i) | i <- [0..populationSize-1]]
  let inertia = inertiaWeight*inertiaDecay
  updatedSnakeParticles <- mapM (\particle -> updateSnakeParticle sBest inertia particle) newPopulation
  particleSwarm (SnakeSwarm updatedSnakeParticles sBest) inertia (iteration - 1)
    where updateParticleBest p@(SnakeParticle pos vel _ val) newVal = do
            if newVal > val then return (SnakeParticle pos vel pos newVal) else return p

-- | Helper
toSnakeNet :: [Float] -> SnakeNet
toSnakeNet c = brainFromChromosome c inputNeurons hiddenNeurons outputNeurons

initializeSwarm :: Int -> IO SnakeSwarm
initializeSwarm popSize = do
  population <- replicateM popSize initializeSnakeParticle
  return $ SnakeSwarm population (position (head population))

initializeSnakeParticle :: IO SnakeParticle
initializeSnakeParticle = do
  g <- newStdGen
  let nmbrOfVariables = ((inputNeurons+1)*hiddenNeurons + (hiddenNeurons+1)*outputNeurons)
  let position = take nmbrOfVariables $ randomRs ((-networkRange), networkRange) g
  velocity <- replicateM nmbrOfVariables initialVelocity
  return $ SnakeParticle position velocity position 0.0
    where initialVelocity = randomRIO (0.0,maxVelocity-0.01)

evaluateSnake :: SnakeNet -> IO Float
evaluateSnake s = startGame s

updateSnakeParticle :: [Float] -> Float -> SnakeParticle -> IO SnakeParticle
updateSnakeParticle swarmBest inertia (SnakeParticle pos velocity bestVal val) = do
  q <- randomRIO (0.0,1.0)
  r <- randomRIO (0.0,1.0)
  let newVelocity = map limitVelocity $ zipWith (+) (map (inertia*) velocity) $ zipWith (+) (cogTerm q) (socTerm r)
  let newPosition = zipWith (+) pos newVelocity
  return $ SnakeParticle newPosition newVelocity bestVal val
    where cogTerm q = (map (cognitiveTerm*q*) (zipWith (-) bestVal pos)) :: [Float]
          socTerm r = (map (cognitiveTerm*r*) (zipWith (-) swarmBest pos)) :: [Float]
          limitVelocity v = if v < 0 then max v (-maxVelocity) else min v maxVelocity
