module HR_Lab9_Practice where

import Data.Ratio ((%)) 
import Text.Printf

main = readLn >>= printf "%.3f\n" . birthday

birthday :: Integer -> Double
birthday n = 1 - (365 `pick` (365 - n + 1)) // (365^n)

pick :: (Integral a) => a -> a -> a
n `pick` k = product [k..n]

(//) :: (Fractional a) => Integer -> Integer -> a
a // b = fromRational (a % b)

