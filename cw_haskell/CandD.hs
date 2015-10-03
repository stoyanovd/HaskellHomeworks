-- Реализовать структуру данных.
-- Реализовать операцию вставки в дерево, узлы которого ещё хранят
--   своего родителя.

-- Реализовать свою версию Show не через deriving

module CandD where

import qualified Data.List

data Tree a = Leaf | Node a (Tree a) (Tree a) (Tree a)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf Leaf
insert x tree@(Node y anc l r)
    | x < y = Node y tree (insert x l) r
    | x > y = Node y tree l (insert x r)
    | otherwise = tree


fromList :: (Ord a) => [a] -> Tree a
fromList [] = Leaf
fromList (x:xs) = insert x (fromList xs)

-- красивый вывод дерева
treeIndent Leaf = ["-- /-"]
treeIndent (Node k _ lb rb) =
    ["--" ++ show k] ++
    map ("  |" ++) ls ++
    ("  `" ++ r) : map ("   " ++) rs
    where
        (r:rs) = treeIndent rb
        ls     = treeIndent lb

printTree :: Show a => Tree a -> IO ()
printTree = mapM_ putStrLn . treeIndent
    
instance Show a => Show (Tree a) where
    show tree = Data.List.intercalate "\n" (treeIndent tree)

a = fromList [1, 4, 3, 6, 2]
