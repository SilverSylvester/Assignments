module Tree where

import Data.List (sort, insert)

-- | Simple, recursive Tree data type
data Tree a = Null | Node a (Tree a) (Tree a)
    deriving (Eq, Show) {- May add a better Show instance -}

-- | _O(log n)_ In-order insertion (left-skewed)
insertT :: (Ord a) => a -> Tree a -> Tree a
insertT x Null = Node x Null Null
insertT x (Node a l r)
    | x <= a     = Node a (insertT x l) r
    | x > a      = Node a l (insertT x r)

-- | _O(n)_ Tree to sorted list.
toList :: (Ord a) => Tree a -> [a]
toList Null = []
toList (Node a l r) = toList l ++ [a] ++ toList r

-- | _O(n*log n)_ Unsorted list to tree
fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insertT Null

-- | _O(n^2 * log n)_ A really bad sorting algorithm
treeSort :: (Ord a) => [a] -> [a]
treeSort = toList . fromList

-- | Insertion sort for comparison
isort :: (Ord a) => [a] -> [a]
isort = foldr insert []

