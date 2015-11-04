module HR_Lab4_Practice where

import Control.Monad (replicateM)
import Data.List (sort)

main = readLn >>= \n -> replicateM n getLine >>= putStrLn . unwords . sort

