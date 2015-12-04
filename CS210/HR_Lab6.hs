module HR_Lab6 where

import Control.Monad (replicateM)

main = readLn >>= flip replicateM getLine >>= \cmds ->
    print' . maximum' . parseIn (map words cmds) $ []
        where print' (Just a) = print a
              print' _        = putStrLn "empty"

              maximum' [] = Nothing
              maximum' xs = Just (maximum xs)

parseIn :: [[String]] -> [Int] -> [Int]
parseIn [] = id
parseIn (xs:xss)
    | head xs == "PUSH" = parseIn xss . ((read (last xs) :: Int):)
    | head xs == "POP"  = parseIn xss . tail'
    | otherwise         = id
    where tail' = drop 1

