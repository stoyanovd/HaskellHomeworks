module MyCachedSumList (CachedSumList, newCachedSumList, add, deleteFirst, deleteNth) where

data CachedSumList a = CachedSumList { cur_sum :: a, list :: [a]}
	deriving (Show)

instance (Num a, Eq a) => Eq (CachedSumList a) where
	l1 == l2 = (cur_sum l1) == (cur_sum l2)

instance (Num a, Ord a) => Ord (CachedSumList a) where
	l1 <= l2 = (cur_sum l1) <= (cur_sum l2)

newCachedSumList :: (Num a) => [a] -> CachedSumList
newCachedSumList list = CachedSumList (sum list) list

add :: (Num a) => a -> CachedSumList -> CachedSumList
add x sL = CachedSumList ((cur_sum sL) + x) (x:(list sL))

deleteFirst :: (Num a) => CachedSumList -> CachedSumList
deleteFirst (CachedSumList 0 []) = CachedSumList 0 []
deleteFirst sL = CachedSumList ((cur_sum sL) - head (list sL)) (tail (list sL))

delete n list = ys ++ (tail zs)
	where (ys, zs) = splitAt n list

deleteNth :: (Num a) => a -> CachedSumList -> CachedSumList
deleteNth n (CachedSumList 0 []) = CachedSumList 0 []
deleteNth n sL = CachedSumList ((cur_sum sL) - ((list sL) !! n)) (delete n (list sL))
