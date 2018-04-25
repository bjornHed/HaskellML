module NetworkUtils where

import Internals

oneHotEncode :: [Int] -> [Array U DIM2 Double]
oneHotEncode input = undefined

oneHotDecode :: [Array U DIM2 Double] -> [Int]
oneHotDecode output = undefined

saveNetwork :: NeuralNetwork -> String -> IO ()
saveNetwork = undefined

loadNetwork :: String -> IO NeuralNetwork
loadNetwork = undefined
