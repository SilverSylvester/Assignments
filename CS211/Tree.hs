module Tree where

data Tree a = Null | Node a (Tree a) (Tree a)
    deriving Show

insertT :: (Ord a) => a -> Tree a -> Tree a
insertT x Null = Node x Null Null
insertT x (Node a l r)
    | x < a     = Node a (insertT x l) r
    | x > a     = Node a l (insertT x r)
    | otherwise = Node a l r

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insertT Null

