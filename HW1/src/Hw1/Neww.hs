module Hw1.Neww where

fa :: (Num a) => a -> a -> a
fa a b = a + b

rr ::  (Num a) => a -> a -> a
rr = fa . fb
    where
        fb x = x * x

y = rr 5
z = rr 5 6