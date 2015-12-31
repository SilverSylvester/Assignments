import Control.Monad.Writer
import Data.Char (digitToInt)

main = getContents >>= print
    . (\(b:b':n:_) -> toBaseN b b' n) . map read . lines

toBaseN :: Integer -> Integer -> Integer -> Integer
toBaseN b b' = fromBase10 b' . toBase10 b

toBase10 :: Integer -> Integer -> Integer
toBase10 b = foldl ((+) . (*b)) 0 . map (fromIntegral . digitToInt) . show

fromBase10 :: Integer -> Integer -> Integer
fromBase10 _ 0 = 0
fromBase10 b n = read . reverse . snd . runWriter $ fromBase10' b n
    where fromBase10' b n
              | n == 0    = return b
              | otherwise = let (q,r) = n `quotRem` b
                            in tell (show r) >> fromBase10' b q

