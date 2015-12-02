module HR_Lab9_Practice where

import Data.Ratio ((%)) 
import Text.Printf

main = readLn >>= printf "%.3f\n" . recBirthday

recBirthday :: Integer -> Double
recBirthday n = 1 - fromRational (recBirthday' n)
    where recBirthday' n
              | n <= 1    = 1
              | otherwise = ((365 - n + 1) % 365) * recBirthday' (n - 1)

