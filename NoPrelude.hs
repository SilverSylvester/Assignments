{-# LANGUAGE NoImplicitPrelude #-}

module NoPrelude where

import GHC.Types (Char)
import System.IO (putStrLn)

-- For full marks, comment out the following imports:

-- import GHC.Classes (Ord  (..))
-- import GHC.Show    (Show (..))
-- import GHC.Types   (Bool, Ordering)

-- Note that the interpreter will get angry at you for trying
-- to print most things unless you put 'putStrLn . show $ '
-- directly before it. I can't define 'print' in file without
-- generating an error, I'm not sure why. Using it in main
-- seems to be fine. Perhaps the fact that I'm calling it print
-- is causing problems?

-- Datatypes --

data Bool = False | True
data Ordering = LT | EQ | GT

-- Type synonyms --

type String = [Char]

-- Classes --

class Eq a where
	(==),(/=) :: a -> a -> Bool

	x == y = not (x /= y)
	x /= y = not (x == y)

class (Eq a) => Ord a where
	(<),(<=),(>=),(>) :: a -> a -> Bool
	compare 		  :: a -> a -> Ordering

	a <  b = compare a b == LT
	a <= b = compare a b /= GT
	a >= b = compare a b /= LT
	a >  b = compare a b == GT

class Show a where
	show :: a -> String

-- Instances --

instance Eq Bool where
	False == False = True
	True  == True  = True
	_     ==   _   = False

instance Eq Ordering where
	LT == LT = True
	EQ == EQ = True
	GT == GT = True
	_  == _  = False

instance Eq Char where
	-- Err...	

instance (Eq a) => Eq [a] where
	[] == []         = True
	[] == _          = False
	_  == []         = False
	(x:xs) == (y:ys) = case (x == y) of
		True  -> xs == ys
		False -> False

instance Ord Bool where
	compare False False = EQ
	compare False True  = LT
	compare True  False = GT
	compare True  True  = EQ

instance Ord Ordering where
	compare LT LT = EQ
	compare EQ EQ = EQ
	compare GT GT = EQ
	compare LT _  = LT
	compare _  LT = GT
	compare GT _  = GT
	compare _  GT = LT

instance (Ord a) => Ord [a] where
	compare [] [] = EQ
	compare _  [] = LT
	compare [] _  = GT
	compare (x:xs) (y:ys) = case (compare x y) of
		LT -> LT
		GT -> GT
		EQ -> compare xs ys

instance Show Char where
	show c = [c]

instance (Show a) => Show [a] where
	show [] = "[]"
	show (x:xs) = "[" ++ show x ++ concatMap ((',':) . show) xs ++ "]"

instance Show Ordering where
	show LT = "LT"
	show EQ = "EQ"
	show GT = "GT"

instance Show Bool where
	show False = "False"
	show True  = "True"

-- Functions --

not :: Bool -> Bool
not False = True
not True = False

(++) :: [a] -> [a] -> [a]
[] ++ ys         = ys
(x:xs) ++ ys = x : (xs ++ ys)

concat :: [[a]] -> [a]
concat = foldl (++) []

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

concatMap f = concat . map f

($) :: (a -> b) -> (a -> b)
f $ x = f x

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)

-- print :: (Show a) => a -> GHC.Types.IO ()
-- print = putStrLn . show
-- Not entirely sure why this doesn't work.

-- Test functions --

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x

const :: a -> b -> a
const a _ = a

reverse :: [a] -> [a]
reverse = foldl (flip (:)) [] -- Nice way to check foldl

all :: (a -> Bool) -> [a] -> Bool
all _       []  = True
all pred (x:xs) = case (pred x) of
	True  -> all pred xs
	False -> False

any :: (a -> Bool) -> [a] -> Bool
any _       []  = False
any pred (x:xs) = case (pred x) of
	False -> any pred xs
	True  -> True

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _ acc    []  = [acc]
scanl f acc (x:xs) = f acc x : scanl f (f acc x) xs 

iterate :: (a -> a) -> a -> [a]
iterate f x = x : scanl (const . f) x [1..]
{- Explanation:

	const :: a -> b -> a
	f     :: a -> a
   	const . f == \x -> const (f x)
   	Note that this function still needs one more parameter,
   	since const takes two but is only supplied one. This
   	extra parameter is the list value, which is discarded.
   	Repeated application therefore gives:
   	x : f x : f (f x) : ... , as required.

   	There are easier ways to define this:
   	iterate f x = x : iterate f (f x), 

-}

-- Required functions -- 

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _       []  = []
takeWhile pred (x:xs) = case (pred x) of
	True  -> x : takeWhile pred xs
	False -> []

filter :: (a -> Bool) -> [a] -> [a]
filter _       []  = []
filter pred (x:xs) = case (pred x) of
	True  -> x : filter pred xs
	False ->     filter pred xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc []     = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc    []  = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) =
	let lt  = sort $ filter (<x)  xs
	    geq = sort $ filter (>=x) xs
	in lt ++ [x] ++ geq

-- Try sorting lists of Ordering or Bool or something:
-- > putStrLn . show $ sort [LT,GT,EQ,EQ,GT,LT,LT]
-- > [LT,LT,LT,EQ,EQ,GT,GT]

-- Can't sort Strings, mainly because I have no idea how to
-- define ordering on Char without refering to Enum and 
-- Int and all sorts of crap I don't want to go near.

-- :t main -> main :: GHC.Types.IO (), according to GHCi.
-- However in defining print, GHCi complains that 
-- GHC.Types.IO is not in scope.
main = putStrLn . show $ sort [LT,GT,EQ,LT,EQ]

-- Ok, the problem is definitely in trying to call the
-- function print.

