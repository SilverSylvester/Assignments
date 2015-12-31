{-# LANGUAGE OverloadedStrings #-}

module Revision_19 where

import Data.List (sortBy)

main = getLine >> getContents
    >>= mapM_ putStrLn . sortBy comp . lines

comp :: (Ord a) => [a] -> [a] -> Ordering
comp a b =
    case compare (maximum a) (maximum b) of
        LT -> LT
        GT -> GT
        _  -> compare a b

