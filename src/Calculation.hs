{-# LANGUAGE ScopedTypeVariables #-}

module Calculation (gradientDescent) where
import Data.Neuron
import Data.Layer
import Data.Network hiding (layers)
import System.Random
import Data.List (zipWith4)

-- MSE function
mse :: [Double] -> [Double] -> Double
mse outputs targets = sum (zipWith (\o t -> (o - t) ** 2) outputs targets) / fromIntegral (length outputs)

-- Neuron gradient update
neuronGradients :: Neuron -> Double -> Double -> [Double] -> Neuron
neuronGradients (Neuron ws b f) output target inputs =
    let delta = (output - target) * derive f output
        dWs = map (* delta) inputs
    in Neuron (zipWith (-) ws dWs) (b - delta) f

-- Backpropagation for a layer
backpropLayer :: Layer -> [Double] -> [Double] -> [Double] -> Layer
backpropLayer (Layer ns) outputs targets inputs = Layer $ zipWith4 neuronGradients ns outputs targets (repeat inputs)

backpropNetwork :: Network -> [Double] -> [Double] -> (Network, Double, [Double])
backpropNetwork (Network layers) inputs targets =
    let -- Forward pass to get outputs of each layer
        allOutputs = scanl (flip fwlayer) inputs layers

        -- Calculate final outputs and deltas
        finalOutputs = last allOutputs

        -- Backpropagate through the network layers in reverse
        newLayers = reverse $ snd $ foldr (\(layer, outputs) (targets', accLayers) ->
                            let newLayer = backpropLayer layer outputs targets' inputs
                            in (outputs, newLayer : accLayers)
                        ) (targets, []) (zip (reverse layers) (reverse allOutputs))
    in (Network newLayers, mse finalOutputs targets, finalOutputs)

gradientDescent :: Int -> Network -> [Double] -> [Double] -> IO Network
gradientDescent 0 network _ _ = return network
gradientDescent x network inputs targets = do
    let (updatedNetwork, loss, finalOutputs) = backpropNetwork network inputs targets
    putStrLn $ "Epochs remaining: " ++ show x ++ "\nLoss: " ++ show loss ++ "\nOutputs: " ++ show finalOutputs ++ "\n"

    gradientDescent (x - 1) updatedNetwork inputs targets