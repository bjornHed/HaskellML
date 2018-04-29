{-# LANGUAGE FlexibleContexts #-}

module Utils (saveNetwork, loadNetwork) where

import Internals
import NeuralNetwork
import qualified Data.ByteString as BS
import qualified Data.Array.Repa as R
import qualified Data.ByteString.Lazy as BSL
import Data.List.Split
import Data.Csv

-- | TODO: save activation function in some way (maybe map)
saveNetwork :: NeuralNetwork -> FilePath -> IO ()
saveNetwork (FFNN weights acti) path = do
    putStrLn $ "Saving network to " ++ path
    BSL.writeFile path $ encode $ map toSavableFormat weights

-- | TODO: load activation in some way.
loadNetwork :: FilePath -> IO NeuralNetwork
loadNetwork path = do
    putStrLn $ "Loading network from file: " ++ path
    content <- readFile path
    let matrices = lines content
        weights = map toRepaMatrix matrices
    return $ FFNN weights sigmoid
  where toRepaMatrix m =
          let (x:y:rest) = splitOn "," m in
          R.fromListUnboxed (R.Z R.:. (dim x) R.:. (dim y)) $ map reader rest
        dim x = round (read x :: Double)
        reader x = read x :: Double

test = do
  let net = initiateNetwork [4,3,2] sigmoid 9
  saveNetwork net "test.csv"

loadActivation :: String -> ActivationFunction
loadActivation function
  | function == "sigmoid" = sigmoid
  | function == "rectifier" = rectifier
  | otherwise = sigmoid

toSavableFormat :: R.Array R.U R.DIM2 Double -> [Double]
toSavableFormat repaArr =
  let (R.Z R.:. x R.:. y) = R.extent repaArr in
  [fromIntegral x, fromIntegral y] ++ (R.toList repaArr)
