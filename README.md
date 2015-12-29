# Assignments
Assignments for CS225, CS210 and CS211
--------------------
Does exactly what it says on the tin.

###### Testing some Markdown features:

Code blocks:

```
reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

fromBase10 :: Integer -> Integer -> Integer
fromBase10 _ 0 = 0
fromBase10 b n = read . reverse . snd . runWriter $ fromBase10' b n
    where fromBase10' b n
              | n == 0    = return b
              | otherwise = let (q,r) = n `quotRem` b
                            in tell (show r) >> fromBase10' b q

```

