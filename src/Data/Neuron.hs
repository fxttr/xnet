{-# LANGUAGE InstanceSigs #-}
module Data.Neuron (relu, sigmoid, activateNeuron, ActivationFunction(..), Neuron(..)) where

import Prelude hiding (exp)
import GHC.Float (exp)

data Neuron = Neuron { weights :: [Double], bias :: Double, activationFunction :: ActivationFunction }
data ActivationFunction = ActivationFunction { activate :: Double -> Double, derive :: Double -> Double }

instance Show Neuron where
  show :: Neuron -> String
  show (Neuron w b _) = 
        "    Weights: " ++ show w ++ "\n" ++
        "    Bias: " ++ show b ++ "\n"

reluFunction :: Double -> Double
reluFunction = max 0

reluFunction' :: Double -> Double
reluFunction' x = if x > 0 then 1 else 0

sigmoidFunction :: Double -> Double
sigmoidFunction x = 1 / (1 + exp (-x))

sigmoidFunction' :: Double -> Double
sigmoidFunction' x = let s = sigmoidFunction x in s * (1 - s)

relu :: ActivationFunction
relu = ActivationFunction { activate = reluFunction, derive = reluFunction' }

sigmoid :: ActivationFunction
sigmoid = ActivationFunction { activate = sigmoidFunction, derive = sigmoidFunction' }

activateNeuron :: Neuron ->  [Double] -> Double
activateNeuron (Neuron ws b f) inputs = activate f (sum (zipWith (*) ws inputs) + b)