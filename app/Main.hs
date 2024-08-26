module Main (main) where

import Lib
import Data.Network
import Data.Neuron
import Data.Layer
import Calculation

initNetwork :: IO Network
initNetwork = do
    inputLayer <- initLayer 6 12 relu
    hiddenLayer1 <- initLayer 14 12 relu
    hiddenLayer2 <- initLayer 12 8 relu
    outputLayer <- initLayer 8 1 sigmoid
    return $ Network [inputLayer, hiddenLayer1, hiddenLayer2, outputLayer]

main :: IO ()
main = do
    network <- initNetwork
    let inputs = [1.0, 0.5, 0.2, 0.4, 0.6, 0.8]
    let targets = [0.324]
    trainedNetwork <- gradientDescent 1000 network inputs targets
    putStrLn "Training finished."
