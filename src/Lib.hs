module Lib
    ( randomList, initLayer
    ) where
import Data.Neuron
import Data.Layer
import Control.Monad ( replicateM )
import System.Random

randomList :: Int -> IO [Double]
randomList n = replicateM n (randomRIO (-1, 1))

initNeuron :: Int -> IO Neuron
initNeuron numInputs = do
    ws <- randomList numInputs
    b <- randomRIO (-1, 1)
    return $ Neuron ws b

initLayer :: Int -> Int -> IO Layer
initLayer numInputs numNeurons = Layer <$> replicateM numNeurons (initNeuron numInputs)