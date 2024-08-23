module Main (main) where

import Lib
import Data.Network

initNetwork :: IO Network
initNetwork = do
    inputLayer <- initLayer 3 12
    hiddenLayer1 <- initLayer 14 12
    hiddenLayer2 <- initLayer 12 8
    outputLayer <- initLayer 8 2
    return $ Network [inputLayer, hiddenLayer1, hiddenLayer2, outputLayer]

main :: IO ()
main = do
    network <- initNetwork
    let inputs = [1.0, 0.5, 0.2]
    let targets = [0.3, 0.4]
    let trainedNetwork = gradientDescent network inputs targets
    print trainedNetwork
