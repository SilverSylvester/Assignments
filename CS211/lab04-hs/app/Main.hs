-- | How to cheat at Scrabble, Haskell style!

module Main where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Array.IO
import Data.Function (on)
import Data.List (sortBy, sortOn)
import Data.Ord (comparing)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Lazy as HM
import System.Exit
import System.IO
import System.Random

main = forever $ do
    -- dictionary <- B.readFile "/home/conor/dictionary.txt"
    dictionary <- B.readFile "/usr/share/dict/words"
    ans <- prompt "Random or custom ('q' to quit)? [r|c] "
    chars <- if ans == "r"
        then scrabbleDraw
        else if ans == "q"
            then exitSuccess
            else B.getLine
    putStr $ "Characters: " ++ B.unpack chars ++ "\n"
    putStrLn "Top ten suggestions:"
    mapM_ B.putStrLn
        $ take 10
        $ sortBy (flip $ comparing B.length)
        $ filter (chars `canForm`) (B.lines dictionary)

-- | Gets character counts for each character in a ByteString.
counts :: B.ByteString -> HM.HashMap Char Int
counts = HM.fromList . map (B.head &&& B.length) . B.group . B.sort

-- | Takes two ByteStrings and checks if some combination of characters
--   in the first string can form the second string.
canForm :: B.ByteString -> B.ByteString -> Bool
canForm = isValid `on` counts
    where isValid :: HM.HashMap Char Int -> HM.HashMap Char Int -> Bool
          isValid chars word
              | null (HM.difference word chars) =
                  not $ any (< 0) $ diffList (HM.toList chars) (HM.toList word)
              | otherwise = False

-- | Rather involved difference list function. Most of the complexity
--   is to make sure the correct characters are being compared on each
--   iteration.
diffList :: (Eq k, Num v) => [(k, v)] -> [(k, v)] -> [v]
diffList [] []         = []
diffList [] ((k,v):ys) = -v : diffList [] ys
diffList ((k,v):xs) [] =  v : diffList xs []
diffList ((k,v):xs) yss@((k',v'):ys)
    | k == k'   = (v - v') : diffList xs ys
    | otherwise = diffList xs yss

--------------------------------------
-- Unecessary but helpful functions --
--------------------------------------

-- | Prompts the user for input in a way that ensures the buffer
--   is flushed properly every time.
prompt :: String -> IO String
prompt msg = do
    putStr msg
    hFlush stdout
    getLine

-- | Draws a random Scrabble hand
scrabbleDraw :: IO B.ByteString
scrabbleDraw = liftM (B.pack . take 7) (shuffle $ replicateEach 5 ['a'..'z'])
    where replicateEach _ []     = []
          replicateEach n (x:xs) = replicate n x ++ replicateEach n xs

-- | Horrendously non-functional, but effective shuffling algorithm.
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

