module HW3.MyTree where

import qualified Data.List
import Control.Applicative

data Tree a = Leaf | Node a (Tree a) (Tree a) (Tree a)

fromList :: (Ord a) => [a] -> Tree a
fromList [] = Leaf
fromList (x:xs) = insert x (fromList xs)

realNode :: Tree a -> Maybe (Tree a)
realNode Leaf = Nothing
realNode t = Just t

insert' :: (Ord a) => a -> Tree a -> Tree a -> Tree a
insert' x Leaf p = Node x p Leaf Leaf
insert' x tree@(Node y _ l r) p
    | x < y = Node y p (insert' x l tree) r
    | x > y = Node y p l (insert' x r tree)
    | otherwise = tree

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = insert' x Leaf Leaf
insert x tree = insert' x tree Leaf

findPlace' :: (Ord a) => a -> Tree a -> Tree a -> Tree a
findPlace' x Leaf p = p
findPlace' x t@(Node y _ l r) _
    | x < y = findPlace' x l t
    | x > y = findPlace' x r t
    | x == y = t

findPlace :: (Ord a) => a -> Tree a -> Tree a
findPlace x t = findPlace' x t Leaf

find :: (Ord a) => a -> Tree a -> Maybe (Tree a)
find x t
    | x == y = Just place
    | otherwise = Nothing
    where
        place@(Node y p l r) = findPlace x t

nodeShow :: (Show a) => Tree a -> [Char]
nodeShow Leaf = "#"
nodeShow (Node x _ _ _) = show x

-- красивый вывод дерева
treeIndent' :: (Show a) => Tree a -> Tree a -> [[Char]]
treeIndent' Leaf p = ["-- /-" ++ "  (" ++ nodeShow p ++ ")"]
treeIndent' t@(Node x p lb rb) _ =
    ["--" ++ show x ++ "  (" ++ (nodeShow p) ++ ")"] ++
    map ("  |" ++) ls ++
    ("  `" ++ r) : map ("   " ++) rs
    where
        (r:rs) = treeIndent' rb t
        ls     = treeIndent' lb t

treeIndent :: (Show a) => Tree a -> [[Char]]
treeIndent t = treeIndent' t Leaf

printTree :: Show a => Tree a -> IO ()
printTree = mapM_ putStrLn . treeIndent

instance Show a => Show (Tree a) where
    show tree = Data.List.intercalate "\n" (treeIndent tree)

next' :: (Ord a) => a -> Tree a -> Maybe (Tree a)
next' x Leaf = Nothing
next' x t@(Node y p l r)
    | x < y = next' x l <|> Just t
    | x >= y = next' x r

prev' :: (Ord a) => a -> Tree a -> Maybe (Tree a)
prev' x Leaf = Nothing
prev' x t@(Node y p l r)
    | x > y = prev' x r <|> Just t
    | x <= y = prev' x l

getValue :: Maybe (Tree a) -> Maybe a
getValue Nothing = Nothing
getValue (Just Leaf) = undefined
getValue (Just (Node x _ _ _ )) = Just x

next :: (Ord a) => Tree a -> a -> Maybe a
next t x = getValue $ next' x t

prev :: (Ord a) => Tree a -> a -> Maybe a
prev t x = getValue $ prev' x t

nextN :: (Ord a) => Tree a -> Integer -> a -> Maybe a
nextN tree n x
    | n < (-1) = (prev tree x) >>= (nextN tree (n + 1))
    | n == (-1) = prev tree x
    | n == 0 = getValue $ find x tree
    | n == 1 = next tree x
    | n > 1 = (next tree x) >>= (nextN tree (n - 1))


et = fromList [1, 4, 3, 6, 2, 14, 17, 3, 18, 5, 7]
