module HR_Lab10_Practice where

import qualified Data.ByteString.Char8 as BC

main = BC.getLine >> BC.getContents >>= mapM_ (BC.putStrLn . BC.reverse) . msort . map BC.reverse . BC.lines

msort :: (Ord a) => [a] -> [a]
msort xs
    | not (null $ tail xs) = merge (msort ls) (msort rs)
    | otherwise            = xs
    where (ls,rs) = split xs
          split xs = (take n xs, drop n xs)
              where n = length xs `div` 2

          merge    []     ys  = ys
          merge    xs     []  = xs
          merge (x:xs) (y:ys)
              | x < y     = x : merge    xs (y:ys)
              | otherwise = y : merge (x:xs)   ys

