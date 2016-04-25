module CW1.A where

import Data.List

f :: (Integral a) => [a] -> [Int]
f = filter even . findIndices even


e = [6,5,4,4,3,2,2,1]
