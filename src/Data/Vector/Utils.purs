module Data.Utils where

import Prelude

norm :: Number -> Number -> Number -> Number
norm a b x = (x - a) / (b - a)

lerp :: Number -> Number -> Number -> Number
lerp a b x = x * (b - a) + a

linearMap :: Number -> Number -> Number -> Number -> Number -> Number
linearMap a b a' b' x = lerp a' b' (norm a b x)
