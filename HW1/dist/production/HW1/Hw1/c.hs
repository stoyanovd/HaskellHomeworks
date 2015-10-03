mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)


merge :: (Ord a) => ([a], [a]) -> [a]
merge ([], ys) = ys
merge (xs, []) = xs
merge (x:xs, y:ys)
        | x <= y = x:merge (xs, y:ys)
        | otherwise = y:merge (x:xs, ys)


mergeSort :: (Ord a) => [a] -> [a]
mergeSort a
        | length a < 2 = a
        | otherwise  = merge (mapTuple mergeSort (splitAt ((length a) `div` 2) a))


