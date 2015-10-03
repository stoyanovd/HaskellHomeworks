module GoTest where

--import Criterion.Main
import System.Random

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = fmap (take n . randomRs (from, to) . mkStdGen) randomIO


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

