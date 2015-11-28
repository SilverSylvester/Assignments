module HR_Lab7_Practice where

main = getLine >>= mapM_ print . foldl (flip insertQ) [] . map readInt . words
    where readInt = read :: String -> Int
    
insertQ :: (Ord a) => a -> [a] -> [a]
insertQ x [] = [x]
insertQ x (y:ys) = case compare x y of
    LT -> x : y:ys
    EQ -> y : x:ys
    GT -> y : insertQ x ys

