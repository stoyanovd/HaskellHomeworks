module B (CachedSumList, newCachedSumList, add, deleteFirst, deleteNth) where

data CachedSumList a = CachedSumList { curSum :: a, list :: [a]} deriving (Show)

instance (Num a, Eq a) => Eq (CachedSumList a) where
 l1 == l2 = curSum l1 == curSum l2

instance (Num a, Ord a) => Ord (CachedSumList a) where
 l1 <= l2 = curSum l1 <= curSum l2

newCachedSumList :: (Num a) => [a] -> CachedSumList a
newCachedSumList list = CachedSumList (sum list) list

add :: (Num a) => a -> CachedSumList a -> CachedSumList a
add x sL = CachedSumList ((curSum sL) + x) (x:(list sL))

deleteFirst :: (Num a) => CachedSumList a -> CachedSumList a
deleteFirst (CachedSumList _ []) = CachedSumList 0 []
deleteFirst sL = CachedSumList ((curSum sL) - head (list sL)) (tail (list sL))

delete n list = ys ++ tail zs
 where (ys, zs) = splitAt n list

deleteNth :: (Num a) => a -> CachedSumList a -> CachedSumList a
deleteNth n (CachedSumList _ []) = CachedSumList 0 []
deleteNth n sL = CachedSumList ((curSum sL) - ((list sL) !! n)) (delete n (list sL))
