module HR_Lab9 where

main = getLine >>= putStrLn . isPalindrome

isPalindrome :: (Eq a) => [a] -> String
isPalindrome []  = "TRUE"
isPalindrome [a] = "TRUE"
isPalindrome (x:xs)
    | x == last xs = isPalindrome (init xs)
    | otherwise    = "FALSE"

-----------------------------------------

main' = getContents >>= print . (\(x:y:_) -> gcd' x y) . map readInteger . lines
    where readInteger = read :: String -> Integer

gcd' :: (Integral a) => a -> a -> a
gcd' a 0 = a
gcd' a b = gcd b (mod a b)

