{-# LANGUAGE InstanceSigs #-}
module Data.Layer (Layer(..), fwlayer) where

import Data.Neuron ( Neuron, activateNeuron )

newtype Layer = Layer { neurons :: [Neuron] }

instance Show Layer where
    show :: Layer -> String
    show (Layer ns) =
        "Neurons:\n" ++ concatMap showNeuron ns
        where
          showNeuron neuron = show neuron ++ "\n"

fwlayer :: Layer -> [Double] -> [Double]
fwlayer (Layer ns) inputs = map (`activateNeuron` inputs) ns