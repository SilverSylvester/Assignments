module Main where

import Control.Arrow ((&&&), first, second)
import qualified Data.ByteString.Char8 as BS
import Data.Function (on)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.PQueue.Prio.Min as Q
import Text.Printf

-- | Huffman-encodes a file and displays information about the
--   encoding, including bit representations of each character,
--   a comparison between the old size of the file and the new
--   size, and the percentage compression achieved.
main = do
    contents <- BS.getContents
    let charFreq   = encode contents
    let decoded    = decode $ huffman charFreq
    let asciiLen   = BS.length contents * 7
    let huffmanLen = huffLength decoded charFreq
    putStrLn $ pp $ sortBy (comparing (length . snd)) decoded
    putStr "Old size: " >> printf "%.3f KiB\n" (asciiLen // 8192)
    putStr "New size: " >> printf "%.3f KiB\n" (huffmanLen // 8192)
    putStr "As a percentage of the old size: "
    printf "%.3f%%\n" (100 * huffmanLen // asciiLen)
        where (//) :: (Integral a) => a -> a -> Double
              (//) = (/) `on` fromIntegral

---------------------------------------------
-- Data types, Type synonyms and instances --
---------------------------------------------

-- | Data type representing a bit. Purely for convenience.
data Bit = Zero | One

instance Show Bit where
    show Zero = "0"
    show One  = "1"

type HuffCode a = [(a,[Bit])]

type CharWeights = [(Char, Int)]

-- | Data type representing a Huffman tree. Only stores data at
--   the leaves.
data HuffmanTree a = Null | Leaf a | Node (HuffmanTree a) (HuffmanTree a)
    deriving Show

---------------
-- Functions --
---------------

-- | Takes a 'ByteString' and very efficiently returns a list
--   of characters with their frequencies.
encode :: BS.ByteString -> CharWeights
encode = map (BS.head &&& BS.length)
       . BS.group
       . BS.sort

-- | Takes CharWeights and constructs its Huffman tree.
huffman :: CharWeights -> HuffmanTree Char
huffman = build . Q.fromList . map (\(c,w) -> (w, Leaf c))
    where build q =
              case Q.minViewWithKey q of
                  Nothing -> Null
                  Just ((w,c), q') ->
                      case Q.minViewWithKey q' of
                          Nothing -> c
                          Just ((w',c'), q'') ->
                              build $ Q.insert (w + w') (Node c c') q''

-- | Builds each characters Huffman code, given its Huffman tree.
decode :: HuffmanTree a -> HuffCode a
decode = decode' []
    where decode' _ Null        = []
          decode' bs (Leaf a)   = [(a,bs)]
          decode' bs (Node l r) = map (second (Zero:)) (decode' bs l) ++
                                  map (second (One:)) (decode' bs r)

-- | Pretty printing codes
pp :: (Show a) => HuffCode a -> String
pp = unlines . map (\(x,bs) -> show x ++ ": " ++ concatMap show bs)

-- | Takes generated Huffman codes and a list of character/weight
--   pairs and returns the new size of the string in bits
huffLength :: (Ord a) => HuffCode a -> CharWeights -> Int
huffLength c cw = 
    sum $ zipWith (*) (map (length . snd) $ sortBy (comparing fst) c)
                      (map snd $ sortBy (comparing fst) cw)


