{-# LANGUAGE InstanceSigs #-}
module Data.Network (Network(..), fwnetwork) where

import Data.Layer ( Layer(..), fwlayer )

newtype Network = Network { layers :: [Layer] }

instance Show Network where
    show :: Network -> String
    show (Network ls) =
        "Layers:\n" ++ concatMap showLayer ls
        where
          showLayer layer = show layer ++ "\n"

fwnetwork :: Network -> [Double] -> [Double]
fwnetwork (Network ls) inputs = foldl (flip fwlayer) inputs ls