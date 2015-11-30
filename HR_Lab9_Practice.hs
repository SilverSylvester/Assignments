module HR_Lab9_Practice where

import Data.Ratio ((%)) 
import Text.Printf

main = readLn >>= printf "%.3f\n" . birthday

birthday :: Integer -> Double
birthday n = 1 - (365 `p` (365 - n + 1)) // (365^n)

p :: (Integral a) => a -> a -> a
n `p` k = product [k..n]

-- Ensures minimal loss of floating point precision
(//) :: (Fractional a) => Integer -> Integer -> a
a // b = fromRational (a % b)

