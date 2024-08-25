module Data.Network (Network(..), fwnetwork) where

import Data.Layer ( Layer(..), fwlayer )

newtype Network = Network { layers :: [Layer] }

fwnetwork :: Network -> [Double] -> [Double]
fwnetwork (Network ls) inputs = foldl (flip fwlayer) inputs ls