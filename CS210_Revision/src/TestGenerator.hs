module Main where

import Control.Monad (liftM)
import System.Random
import System.Environment
import Test.QuickCheck

test8 :: IO [Int]
test8 = liftM (randomRs (-1000,1000)) newStdGen

tests :: [IO [Int]]
tests = [test1,test2,test3,test4,test5,test6,test7,test8]

main = getArgs >>= \(n:ns:_) -> (tests !! (read n - 1))
                >>= mapM_ (putStr . (++ " ") . show) . take (read ns)

test1 = undefined
test2 = undefined
test3 = undefined
test4 = undefined
test5 = undefined
test6 = undefined
test7 = undefined

