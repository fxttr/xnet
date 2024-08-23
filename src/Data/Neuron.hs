module Data.Neuron (relu, relu', sigmoid, sigmoid', reluNeuron, sigmoidNeuron, Neuron(..)) where
    
import Prelude hiding (exp)
import GHC.Float (exp)

data Neuron = Neuron { weights :: [Double], bias :: Double } deriving Show

relu :: Double -> Double
relu = max 0

relu' :: Double -> Double
relu' x = if x > 0 then 1 else 0

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Double -> Double
sigmoid' x = let s = sigmoid x in s * (1 - s)

reluNeuron :: Neuron -> [Double] -> Double
reluNeuron (Neuron ws b) inputs = relu (sum (zipWith (*) ws inputs) + b)

sigmoidNeuron :: Neuron -> [Double] -> Double
sigmoidNeuron (Neuron ws b) inputs = relu (sum (zipWith (*) ws inputs) + b)