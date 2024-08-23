module Data.Layer (Layer(..), layer) where

import Data.Neuron ( Neuron, reluNeuron )

newtype Layer = Layer { neurons :: [Neuron] }

layer :: Layer -> [Double] -> [Double]
layer (Layer ns) inputs = map (`reluNeuron` inputs) ns