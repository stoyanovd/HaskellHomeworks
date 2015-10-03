module MyTree where

data Tree a = Leaf | Node a (Tree a) (Tree a)

treeFromList [] = Leaf
treeFromList (x:xs) = insert x (treeFromList xs)

find x Leaf = Leaf
find x (Node y l r)
	| x == y = (Node y l r)
	| x < y = find x l
	| otherwise = find x r

findLeft (Node x Leaf Leaf) = x
findLeft (Node x l r) = findLeft l

findRight (Node x Leaf Leaf) = x
findRight (Node x l r) = findRight r

delete x Leaf = Leaf
delete x (Node y Leaf Leaf)
	| x == y = Leaf
	| otherwise = (Node y Leaf Leaf)

delete x (Node y Leaf r)
	| x == y = 
		let lx = findLeft r
		in Node lx Leaf (delete lx r)
	| x < y = Node y Leaf r
	| otherwise = Node y Leaf (delete x r)

delete x (Node y l r)
	| x == y = 
		let rx = findRight l
		in Node rx (delete rx l) r
	| x < y = Node y (delete x l) r
	| otherwise = Node y l (delete x r)



module E where

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
