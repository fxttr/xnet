module Data.Layer (Layer(..), fwlayer) where

import Data.Neuron ( Neuron, activateNeuron )

newtype Layer = Layer { neurons :: [Neuron] }

fwlayer :: Layer -> [Double] -> [Double]
fwlayer (Layer ns) inputs = map (`activateNeuron` inputs) ns