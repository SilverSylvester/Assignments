module Ex1 where

-- Recreating some simple library functions

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' pred (x:xs)
	| pred x	= x : takeWhile' pred xs
	| otherwise =     []

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' pred (x:xs)
	| pred x	= x : filter' pred xs
	| otherwise =     filter' pred xs

gcd' :: (Integral a) => a -> a -> a
gcd' 0 0 = error "gcd(0,0) is undefined"
gcd' a 0 = a
gcd' a b = gcd' b (mod a b)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc []     = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc    []	 = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

sort' :: (Ord a) => [a] -> [a]
sort' []     = []
sort' (x:xs) =
	let lt   = sort' $ filter' (<x)  xs
	    geq  = sort' $ filter' (>=x) xs
	in  lt ++ [x] ++ geq

