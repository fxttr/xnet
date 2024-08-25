module Calculation () where
import Data.Neuron
import Data.Layer
import Data.Network
import System.Random
import Data.List (transpose, zipWith3)


mse :: [Double] -> [Double] -> Double
mse outputs targets = sum (zipWith (\o t -> (o - t) ** 2) outputs targets) / fromIntegral (length outputs)

neuronGradients :: Neuron -> [Double] -> Double -> (Neuron, [Double])
neuronGradients (Neuron ws b f) inputs delta =
    let dWs = map (* delta) inputs
        dB = delta
    in (Neuron (zipWith (-) ws dWs) (b - dB) f, dWs)

backpropLayer :: Layer -> [[Double]] -> [Double] -> (Layer, [[Double]])
backpropLayer (Layer ns) inputs deltas = (Layer (map fst neuronsUpdated), transpose deltasList)
  where
    neuronsUpdated = zipWith3 neuronGradients ns inputs deltas
    deltasList = map snd neuronsUpdated

backpropNetwork :: Network -> [Double] -> [Double] -> Network
backpropNetwork (Network ls) inputs targets =
    let outputs = foldl fwlayer ls inputs
        deltas = zipWith (\o t -> (o - t) * relu' o) outputs targets
        (newLayers, _) = foldr (\l (accInputs, d) -> 
            let (nl, nd) = backpropLayer l accInputs d 
            in (nl:accInputs, nd)
          ) ([outputs], deltas) (reverse ls)
    in Network (reverse (tail newLayers))

gradientDescent :: Network -> [Double] -> [Double] -> Network
gradientDescent = backpropNetwork