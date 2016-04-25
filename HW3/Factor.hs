module Factor where

import qualified Data.List

{-primes :: Int -> [Int]
primes 2 = [2]
primes n = (primes (n - 1)) ++ 
    if all ((/= 0) . (mod n)) (primes (n - 1)) then [n] else []
-}

divideSingle' :: Int -> Int -> [Int]
divideSingle' _ 1 = []
divideSingle' r x
    | x < r = [x]
    | x `mod` r == 0 = (r:(divideSingle' r (x `div` r)))
    | otherwise = divideSingle' (r + 1) x


divideSingle :: Int -> [Int]
divideSingle = divideSingle' 2


factor :: [Int] -> [Int]
factor xs = Data.List.sort $ xs >>= divideSingle