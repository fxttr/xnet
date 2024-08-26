module Lib
    ( randomList, initLayer
    ) where
import Data.Neuron
import Data.Layer
import Control.Monad ( replicateM )
import System.Random

randomList :: Int -> IO [Double]
randomList n = replicateM n (randomRIO (-1, 1))

initNeuron :: Int -> ActivationFunction -> IO Neuron
initNeuron numInputs f = do
    ws <- randomList numInputs
    b <- randomRIO (-1, 1)
    return $ Neuron ws b f

initLayer :: Int -> Int -> ActivationFunction -> IO Layer
initLayer numInputs numNeurons f = Layer <$> replicateM numNeurons (initNeuron numInputs f)