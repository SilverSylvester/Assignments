module HR_Lab7 where

import Control.Monad (replicateM)

main = getContents >>= \cmds ->
    putStrLn . middle . toList . parseIn (map words (lines cmds)) $ empty

middle :: [String] -> String
middle xs
    | null xs          = "empty"
    | even (length xs) = xs !! ((length xs) `div` 2 - 1)
	| otherwise        = xs !! ((length xs) `div` 2)
	
parseIn :: [[String]] -> (Queue String -> Queue String)
parseIn [] = id
parseIn (xs:xss)
	| head xs == "INSERT" = parseIn xss . enq (last xs)
	| head xs == "REMOVE" = parseIn xss . deq
	| otherwise           = id

-- Queue interface --

data Queue a = Queue [a] [a] deriving Show

empty :: Queue a
empty = Queue [] []

toList :: Queue a -> [a]
toList (Queue xs ys) = xs ++ reverse ys

fromList :: [a] -> Queue a
fromList xs = Queue xs []

enq :: a -> Queue a -> Queue a
enq y (Queue xs ys) = Queue xs (y:ys)

deq :: Queue a -> Queue a
deq (Queue [] [])     = Queue [] []
deq (Queue [] ys)     = deq (Queue (reverse ys) [])
deq (Queue (x:xs) ys) = Queue xs ys

