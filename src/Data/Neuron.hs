module Data.Neuron (relu, relu', sigmoid, sigmoid', activateNeuron, Neuron(..)) where

import Prelude hiding (exp)
import GHC.Float (exp)

data Neuron = Neuron { weights :: [Double], bias :: Double, activationFunction :: Double -> Double }

relu :: Double -> Double
relu = max 0

relu' :: Double -> Double
relu' x = if x > 0 then 1 else 0

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Double -> Double
sigmoid' x = let s = sigmoid x in s * (1 - s)

activateNeuron :: Neuron ->  [Double] -> Double
activateNeuron (Neuron ws b f) inputs = f (sum (zipWith (*) ws inputs) + b)