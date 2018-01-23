module SnakeUtils where

import Data.Matrix as M;

------------------------
--   Snake specifics  --
------------------------
snakeName = "Functional Snake"

---------------------------
--  Websocket constants  --
---------------------------
-- TODO
host = "snake.cygni.se"
port = "80"
venue = "training"

--------------------------
--   Network constants  --
--------------------------
networkRange = 6.0 :: Float
hiddenNeurons = 200 :: Int
inputNeurons = 400 :: Int
outputNeurons = 2 :: Int

---------------------------------------------
--   ParticleSwarmOptimization constants   --
---------------------------------------------
cognitiveTerm = 2.0 :: Float
socialTerm = 2.0 :: Float
weightInertia = 1.4 :: Float
inertiaDecay = 0.9 :: Float
inertiaMin = 0.5 :: Float
populationSize = 30 :: Int
iterations = 1 :: Int
maxVelocity = networkRange*2

data SnakeNet = SnakeNet {
  weights :: [Matrix Float],
  bias    :: [[Float]] -- Since there can be different number of nodes in each hidden layer
} deriving ( Show )

data Direction = AHEAD | LEFT | RIGHT
  deriving ( Show, Enum, Eq )
