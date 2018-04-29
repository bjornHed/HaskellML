module Utils where

import Internals

oneHotEncode :: Int -> [Double]
oneHotEncode input =
    let maxi = maximum input in
    Prelude.map (transform maxi) input
  where transform maxi val =
          (replicate val 0.0) ++ (1.0:(replicate (maxi-val) 0.0))


oneHotDecode :: [Array U DIM2 Double] -> [Int]
oneHotDecode output = undefined

saveNetwork :: NeuralNetwork -> FilePath -> IO ()
saveNetwork = undefined

loadNetwork :: FilePath -> IO NeuralNetwork
loadNetwork = undefined
