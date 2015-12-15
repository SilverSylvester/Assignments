module HR_Lab11_Practice where

import Control.Monad.Writer
import Data.Char (digitToInt)

type Base = Integer

main = getContents >>= print . (\(b:b':n:_) -> toBaseN b b' n) . map read . lines

toBaseN :: Base -> Base -> Integer -> Integer
toBaseN b b' = read . reverse . snd . runWriter . fromBase10 b' . toBase10 b

toBase10 :: Base -> Integer -> Integer
toBase10 b = foldl ((+) . (*b)) 0 . map (fromIntegral . digitToInt) . show

fromBase10 :: Base -> Integer -> Writer String Integer
fromBase10 b n
    | n == 0    = return b
    | otherwise = let (q,r) = n `quotRem` b
                  in do tell (show r)
                        fromBase10 b q

