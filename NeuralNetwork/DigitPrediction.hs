{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Array.Repa as R
import NeuralNetwork
import Internals
import System.Random

main :: IO ()
main = do
    csvData <- BL.readFile "train_cp.csv"
    case decode HasHeader csvData of
        Left err -> putStrLn err
        Right (v :: V.Vector [String]) -> do
          seed <- randomIO
          let lists = V.toList v
              (intLists :: [[Double]]) = map (map Prelude.read) lists
              trainingData = map (\x -> (map (\(y::Double) -> y/255) (tail x), onehot (head x))) intLists
              network@(FFNN y _) = initiateNetwork [784,365,10] sigmoid seed
          putStrLn "Started training..."
          newNet@(FFNN w a) <- trainNetwork network trainingData (TS 1 False 0.0 True 1)
          putStrLn "Training done"
          validate newNet
          res <- feedForwardP (fst (trainingData !! 1)) newNet
          let result = R.toList res
          print $ "Desired result: " ++ show (snd (trainingData !! 1))
          print $ map (\x -> x/(sum result)) result

onehot :: Double -> [Double]
onehot v =
  let val = round v in
  (replicate val 0.0) ++ (1.0:(replicate (9-val) 0.0))

validate :: NeuralNetwork -> IO ()
validate network = do
      csvData <- BL.readFile "validate.csv"
      case decode NoHeader csvData of
        Left err -> putStrLn err
        Right (v :: V.Vector [String]) -> do
          let lists = V.toList v
              (doubleLists :: [[Double]]) = map (map Prelude.read) lists
              valData = map (\x -> (map (\(y::Double) -> y/255) (tail x), (head x))) doubleLists
          correct <- (mapM (try network) valData)
          print $ sum correct
  where try network (input, target) = do
          out <- getResult input network
          let output = softmax out
              maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]
        --  print output
          putStrLn $ "Belived it was a " ++ show (maxIndex output) ++ " and it was a " ++ show (round target)
          if (maxIndex output) == (round target)
          then return 1
          else return 0
        getResult inp net = do
          output <- feedForwardP inp net
          return $ R.toList $ output
        softmax xs = map (\x -> x/(sum xs)) xs
