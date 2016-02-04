module Tree where

import Control.Monad
import Data.List (sort, insert)
import Data.Tree hiding (Tree)
import System.Random
import Data.Array.IO

-- | Simple, recursive Tree data type
data Tree a = Null | Node a (Tree a) (Tree a)
    deriving (Eq, Show) {- May add a better Show instance -}

-- | _O(log n)_ In-order insertion (left-skewed)
insertT :: (Ord a) => a -> Tree a -> Tree a
insertT x Null = Node x Null Null
insertT x (Node a l r)
    | x <= a = Node a (insertT x l) r
    | x > a  = Node a l (insertT x r)

-- | _O(n)_ Tree to sorted list.
toList :: (Ord a) => Tree a -> [a]
toList Null         = []
toList (Node a l r) = toList l ++ [a] ++ toList r

-- | _O(n*log n)_ Unsorted list to tree
fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insertT Null

-- | _O(n*log n) amortized_ A really bad sorting algorithm if
--   the list is sorted, a pretty good sorting algorithm if
--   the list is random.
treeSort :: (Ord a) => [a] -> [a]
treeSort = toList . fromList

-- | Insertion sort for comparison
isort :: (Ord a) => [a] -> [a]
isort = foldr insert []

----------------
-- Extra shit --
----------------

-- | /O(n)/ Shuffles a list of any type using
--   @randomRIO@.
shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- newArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
    where
        n = length xs
        newArray :: Int -> [a] -> IO (IOArray Int a)
        newArray n = newListArray (1,n)

