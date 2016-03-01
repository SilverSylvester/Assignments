module Main where

import Control.Monad
import Data.Array.IO
import Data.List
import Data.Ord (comparing)
import System.Exit
import System.IO
import System.Random

main = forever $ do
    ans <- prompt "Random or custom (or quit)? [r/c/q] "
    chars <- if ans == "r" || ans == "R"
        then scrabbleDraw
        else if ans == "q"
            then exitSuccess
            else getLine
    putStrLn $ "Characters: " ++ chars
    putStrLn "Top 10 suggestions:"
    dict <- dictionary
    mapM_ putStrLn
        $ take 10
        $ sortBy (flip $ comparing length)
        $ findWords (combinations chars) dict

-- | Get's all possible combinations of available characters in a
--   string.
combinations :: String -> [String]
combinations = setsort . concatMap permutations . subsequences

-- | Loads a dictionary file for processing
dictionary :: IO [String]
dictionary = liftM lines (readFile "/usr/share/dict/words")
-- dictionary = liftM lines (readFile "/home/conor/dictionary.txt")

-- | Custom nub function (with tighter typeclass constraint). Has
--   the nice (and necessary) side effect of sorting the list.
nub' :: Ord a => [a] -> [a]
nub' = map head . group . sort

-- | Matches two sorted lists and efficiently returns a list of
--   those in the left list that appear in the right list.
findWords :: Ord a => [a] -> [a] -> [a]
findWords [] _ = []
findWords _ [] = []
findWords (x:xs) (y:ys)
    | x <  y = findWords xs (y:ys)
    | x == y = x : findWords xs ys
    | x >  y = findWords (x:xs) ys

-- | Draws a scrabble 'hand' from a shuffled list of characters.
scrabbleDraw :: IO String
scrabbleDraw = liftM (take 7) (shuffle $ replicateEach 5 ['a'..'z'])
    where replicateEach _ []     = []
          replicateEach n (x:xs) = replicate n x ++ replicateEach n xs

-- | Custom mergesort, ignoring duplicate elements on merge.
setsort :: (Ord a) => [a] -> [a]
setsort xs
    | null (tail xs) = xs
    | otherwise      = merge (setsort ls) (setsort rs)
    where (ls,rs) = splitAt (length xs `div` 2) xs
              
          merge [] ys = ys
          merge xs [] = xs
          merge xss@(x:xs) yss@(y:ys) =
              case compare x y of
                  LT -> x : merge xs yss
                  EQ -> x : merge xs ys
                  GT -> y : merge xss ys

-- | Yucky but effective shuffling algorithm.
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

-- | Prompts the user for input in a way that ensures the buffer
--   is flushed properly every time.
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

