module Data.Network (Network(..), network) where

import Data.Layer ( Layer(..), layer )

newtype Network = Network { layers :: [Layer] }

network :: Network -> [Double] -> [Double]
network (Network ls) inputs = foldl (flip layer) inputs ls