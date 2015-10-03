module RandomListUtils where
	
	import System.Random
	
	randomIntList :: Int -> Int -> Int -> IO [Int]
	randomIntList n from to = fmap (take n . randomRs (from, to) . mkStdGen) randomIO
