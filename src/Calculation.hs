module Calculation () where
import Data.Neuron
import Data.Layer
import Data.Network
import System.Random

mse :: [Double] -> [Double] -> Double
mse outputs targets = sum (zipWith (\o t -> (o - t) ** 2) outputs targets) / fromIntegral (length outputs)

neuronGradients :: Neuron -> [Double] -> Double -> (Neuron, [Double])
neuronGradients (Neuron ws b) inputs delta =
    let dWs = map (* delta) inputs
        dB = delta
    in (Neuron (zipWith (-) ws dWs) (b - dB), dWs)

backpropLayer :: Layer -> [Double] -> [Double] -> ([Layer], [Double])
backpropLayer (Layer ns) inputs deltas = (map fst neuronsUpdated, map sum $ transpose deltasList)
  where
    neuronsUpdated = zipWith3 neuronGradients ns inputs deltas
    deltasList = map snd neuronsUpdated
    sum xs = foldl (+) 0 xs

backpropNetwork :: Network -> [Double] -> [Double] -> Network
backpropNetwork (Network ls) inputs targets =
    let outputs = foldl layer inputs ls
        deltas = zipWith (\o t -> (o - t) * relu' o) outputs targets
        newLayers = foldl (\(acc, d) l -> let (nl, nd) = backpropLayer l acc d in (nl:acc, nd)) ([], deltas) ls
    in Network (reverse newLayers)

gradientDescent :: Network -> [Double] -> [Double] -> Network
gradientDescent = backpropNetwork