{-# LANGUAGE FlexibleContexts #-}
{-
########################################
| K-means clustering algorithm in 2D.  |
|                                      |
| Björn Hedström, 2017                 |
|                                      |
| Standard k-means clustering for 2D   |
| as a hobby project. Does however not |
| work in parallell yet. Should of     |
| course work for more dimensions      |
| later.                               |
########################################
-}
module Kmeans2D (kmeans, pkmeans) where

import System.Random
import GHC.Conc (numCapabilities)
import Data.List
import Data.List.Split
import Control.Monad.Par
import Criterion.Main

dataset :: [(Double,Double)]
dataset = [(1,2),(1,3),(2,1),(3,1),(8,8),(9,7),(8,7),(7,5)]

main = do
  let inp= map realToFrac (take 100 (randoms (mkStdGen 211570155)) :: [Int] )
  let points = zip inp $ map realToFrac [0..]
  defaultMain
        [
           bench "Sequential Kmeans" (nfIO (kmeans 4 points)),
           bench "Parallel Kmeans" (nfIO (pkmeans 4 points))
         ]

-- | Initial call for the k-means function.
kmeans :: Int -> [(Double,Double)] -> IO [Int]
kmeans k points = do
  means <- generateInitial k points
  let labels = assignLabels points means
  return $ kmeansH points means assignLabels labels

  -- | Initial call for the parallel k-means function.
pkmeans :: Int -> [(Double,Double)] -> IO [Int]
pkmeans k points = do
  means <- generateInitial k points
  let labels = runPar $ pAssignLabels points means
  return $ pkmeansH points means pAssignLabels labels

-- | Recursive call which updates the means and labels until
-- | the algorithm converges.
kmeansH ::
  [(Double,Double)] ->
  [(Double,Double)] ->
  ([(Double,Double)] -> [(Double,Double)] -> [Int]) ->
  [Int] -> [Int]
kmeansH points means labelAssigner labels =
    if newLabels == labels
    then labels
    else kmeansH points u labelAssigner newLabels
  where u = calculateMeans points (length means) labels
        newLabels = labelAssigner points u

pkmeansH ::
  [(Double,Double)] ->
  [(Double,Double)] ->
  ([(Double,Double)] -> [(Double,Double)] -> Par [Int]) ->
  [Int] -> [Int]
pkmeansH points means labelAssigner labels =
    if newLabels == labels
    then labels
    else pkmeansH points u labelAssigner newLabels
  where u = calculateMeans points (length means) labels
        newLabels = runPar $ labelAssigner points u

-- | The euclidian distance between two 2D-points.
distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1,y1) (x2,y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

-- | Randomizes k initial means from the input.
generateInitial :: Int -> [(Double,Double)] -> IO [(Double,Double)]
generateInitial k points = do
  g <- newStdGen
  let indexes = take k $ randomRs (0, length points-1) g :: [Int]
  return $ map (\x -> points !! x) indexes

-- | Assigns a new label to each point.
assignLabels :: [(Double,Double)] -> [(Double,Double)] -> [Int]
assignLabels points means =
    map (\x -> minInd (map (distance x) means)) points
  where minInd xs = snd $ head $ sort $ zip xs [0..]

-- | Assigns a new label to a point. Performs the operation
-- | in parallel depending on aviliable cores on the machine.
pAssignLabels :: [(Double,Double)] -> [(Double,Double)] -> Par [Int]
pAssignLabels points means
  | numCapabilities == 1 = return $ assignLabels points means
  | otherwise = do
      let chunks = transpose $ chunksOf numCapabilities points
      p <- sequence [spawn(return (assignLabels c means)) | c <- chunks]
      r <- sequence [get i | i <- p]
      return $ concat r

-- | Calculates new means from the labeld points.
calculateMeans :: [(Double, Double)] -> Int -> [Int] -> [(Double, Double)]
calculateMeans points k labels =
    map newMean meanInd
      where
        pointInd = zip points labels
        meanInd = [0..(k-1)]
        values x = filter (\(p,ind) -> ind == x) pointInd
        newMean x =
          (\(z1,z2) -> (z1/(realToFrac (length (values x))), z2/(realToFrac (length (values x)))))
            $ foldr (\(x1,y1) -> \(x2,y2) -> (x1+x2,y1+y2)) (0,0) $ map fst $ values x
