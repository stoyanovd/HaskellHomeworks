module MyTree where

data Tree a = Leaf | Node a (Tree a) (Tree a)

fromList [] = Leaf
fromList (x:xs) = insert x (fromList xs)

insert x Leaf = Node x Leaf Leaf
insert x (Node y l r) 
	| x < y = Node y (insert x l) r
	| x > y = Node y l (insert x r) 
        | otherwise = Node y l r

find x Leaf = Nothing
find x (Node y l r)
	| x == y = (Node y l r)
	| x < y = find x l
	| otherwise = find x r

findLeft Leaf = Nothing
findLeft (Node x Leaf Leaf) = x
findLeft (Node x l r) = findLeft l

findRight Leaf = Nothing
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

-- красивый вывод дерева
printTree :: Show a => Tree a -> IO ()
printTree = mapM_ putStrLn . treeIndent
  where
    treeIndent Leaf           = ["-- /-"]
    treeIndent (Node k lb rb) =
      ["--" ++ show k] ++
      map ("  |" ++) ls ++
      ("  `" ++ r) : map ("   " ++) rs
      where
        (r:rs) = treeIndent rb
        ls     = treeIndent lb

