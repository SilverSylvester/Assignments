module HR_Lab5_Practice where

import Control.Monad (replicateM)
import Data.List (sort)

main = readLn >>= flip replicateM getLine >>= putStrLn . unwords . sort

