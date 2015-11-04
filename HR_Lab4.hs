
main = do
	n <- readLn
	let ans = closest n $ primesTo $ 2*n
	print ans

-- All primes LEQ to given integer.
primesTo :: Integer -> [Integer]
primesTo m = eratos [2..m] where
	eratos [] = []
	eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p + p .. m])

-- Almost set difference. Can deal with infinite lists in arg2.
minus :: (Ord a) => [a] -> [a] -> [a]
minus (x:xs) (y:ys) = case (compare x y) of
	LT -> x : minus xs  (y:ys)
	EQ ->     minus xs     ys
	GT ->     minus (x:xs) ys
minus xs _ = xs

-- Returns first element in list closest to a given value. Must
-- iterate through entire list.
closest :: (Integral a) => a -> [a] -> a
closest _ [] = error "Nothing to compare to"
closest n xs = foldl1 (\x y -> if (abs (x - n) > abs (y - n)) then y else x) xs

