{----------------------------------------------------|
| Genetic Algorithm                                  |
|                                                    |
| Björn Hedström 2017                                |
|                                                    |
| module for a generic genetic algorithm with        |
| binary encoding. As of yet not tested, merly done  |
| as a hobby project.                                |
|----------------------------------------------------}

module GeneticAlgorithm (geneticAlgorithm, geneticAlgorithm') where

import Data.Matrix as M
import Data.Vector as V hiding ( map, sum, zip, maximum, minimum )
import Control.Monad.Random as R
import Data.List as L

-- | A default genetic algorithm with some preset probabilites
-- | for ease of use.
geneticAlgorithm' :: Int -> -- genes per variable
                     Int -> -- variables
                     (Float,Float) -> -- variable range
                     ([Float] -> Float) -> -- fitness-function
                     Int -> -- number of generations
                     IO [Float] -- returned best value
geneticAlgorithm' g v r f gen =
  geneticAlgorithm 30 g v r 0.8 ((1 :: Float)/(fromIntegral (g*v))) 0.75 2 f gen

-- | Runs a genetic algorithm for a given problem.
geneticAlgorithm :: Int -> -- Population size
                    Int -> -- genes per variable
                    Int -> -- variables
                    (Float,Float) -> -- variable range
                    Float -> -- crossover probability
                    Float -> -- mutation probability
                    Float -> -- tournament-select probability
                    Int -> -- tournament size
                    ([Float] -> Float) -> -- fitness-function
                    Int -> -- number of generations
                    IO [Float] -- returned best value
geneticAlgorithm pop genes var range pCross pMute pTour tSize fun generations = do
    population <- initializePopulation' pop (genes*var)
    run population generations
  where run popu 0 = do
          putStr "==] "
          return $ snd $ maximum $ zip (evalPopulation popu) -- result
            (map (\i -> decodeIndividual (popu !! i) var range) [0..pop-1])
        run popu gen = do
            let bestIndividual = snd . maximum $ zip (evalPopulation popu) (map (\i -> popu !! i) [0..pop-1])
            newPop <- Prelude.mapM (breed popu) [1..pop]
            if gen `mod` 100 == 0
              then putStr "=="
              else putStr ""
            run (bestIndividual:(L.tail (L.take pop $ L.concat $ newPop))) (gen - 1)
        evalPopulation population =
          map (\i -> fun (decodeIndividual (population !! i) var range)) [0..pop-1]
        breed popu _ = do
          r <- randomRIO (0 :: Float, 1 :: Float)
          if r < pCross
          then do
            (c1,c2) <- selectTwo popu
            (cross1,cross2) <- crossover (popu !! c1) (popu !! c2)
            mc1 <- mutation cross1 pMute
            mc2 <- mutation cross2 pMute
            return [mc1,mc2]
          else do
            (c1,c2) <- selectTwo popu
            mc1 <- mutation (popu !! c1) pMute
            mc2 <- mutation (popu !! c2) pMute
            return [mc1,mc2]
        selectTwo popu = do
          let fitness = evalPopulation popu
          c1 <- tournamentSelect fitness pTour
          c2 <- tournamentSelect fitness pTour
          return (c1, c2)

-- | Initializes a random population.
-- | Takes a population size (n) and a number of genes (m) for each individual.
-- | Returns a matrix with dimensions (n x m).
-- |
-- | NOTE: Initializes with binary encoding.
initializePopulation :: Int -> Int -> IO (Matrix Int)
initializePopulation popSize genes = do
  std <- newStdGen
  return $ M.fromList popSize genes $ randomRs (0 :: Int, 1 :: Int) std

initializePopulation' :: Int -> Int -> IO [Vector Int]
initializePopulation' popSize genes =
  R.sequence [generateM genes (\x -> randomRIO (0,1)) | i <- [1..popSize]]

-- | Decodes a individual to a specified number of variables.
-- | The variables are returned in a list.
-- |
-- | NOTE: Decodes a binary individual.
decodeIndividual :: Vector Int -> Int -> (Float, Float) -> [Float]
decodeIndividual individual variables (startRange, endRange) =
    map scale $ map decodeVariable [1..variables]
  where decodeVariable var =
          let genesPerVar = (V.length individual) `div` variables in
          let s = force $ unsafeSlice ((var-1)*genesPerVar) genesPerVar individual in
          sum $ map (\i -> (fromIntegral (s V.! i)
            :: Float)*2**(-((fromIntegral i :: Float)+1))) [0..genesPerVar-1]
        scale x = startRange + x*(2*endRange)/(1-2**(-10))

-- | Performs crossover between two given chromosomes.
crossover :: Vector Int -> Vector Int -> IO (Vector Int, Vector Int)
crossover chromosome1 chromosome2 = do
  g <- newStdGen
  let crossoverPoint = fst $ randomR (0, V.length chromosome1) g
  let (c11,c12) = V.splitAt crossoverPoint chromosome1
  let (c21,c22) = V.splitAt crossoverPoint chromosome2
  return (c11 V.++ c22,c21 V.++ c12)

-- | Performs mutation on a chromosome with a given
-- | mutation rate for each gene.
mutation :: Vector Int -> Float -> IO (Vector Int)
mutation chromosome pMute =
  V.mapM mutate chromosome
    where mutate x = do
            r <- randomRIO (0 :: Float, 1)
            if r < pMute
            then return $ abs (x - 1)
            else return x

-- | Tournament select with tournament size = 2.
-- | returns index of winning value.
tournamentSelect :: [Float] -> Float -> IO Int
tournamentSelect fitness pTour = do
  let l = Prelude.length fitness - 1
  g1 <- randomRIO (0,l)
  g2 <- randomRIO (0,l)
  r <- randomRIO (0 :: Float, 1 :: Float)
  if r < pTour
  then return $ snd . maximum $ zip [(fitness !! g1),(fitness !! g2)] [g1,g2]
  else return $ snd . minimum $ zip [(fitness !! g1),(fitness !! g2)] [g1,g2]

-- | Takes the tournament size, a list of parameters and a
-- | probability of selecting the best individual. Returns
-- | the index of the selected individual.
tournamentSelect' :: [Float] -> Float -> Int -> IO Int
tournamentSelect' fitness pTour size = do
  participants <- Prelude.mapM selectRnd [1..size]
  tournament participants
    where selectRnd _ = do
            ind <- randomRIO (0, Prelude.length fitness-1)
            return (fitness !! ind, ind)
          tournament par =
            if Prelude.length par == 1
            then return $ snd $ Prelude.head par
            else do
              r <- randomRIO (0 :: Float, 1:: Float)
              if r < pTour
              then return $ snd $ maximum par
              else tournament (delete (maximum par) par)
