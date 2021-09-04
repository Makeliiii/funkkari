{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
-- listing 7.1
myGCD :: Integral t => t -> t -> t
myGCD a b = if remainder == 0
            then b
            else myGCD b remainder
    where remainder = a `mod` b

-- listing 7.2
sayAmount :: (Eq a, Num a) => a -> [Char]
sayAmount n = case n of
    1 -> "one"
    2 -> "two"
    n -> "a bunch"

-- listing 7.3
sayAmount 1 = "one"
sayAmount 2 = "two"
sayAmount n = "a bunch"

-- quick check 7.3 and Q7.1
myTail :: [a] -> [a]
myTail (x:xs) = xs
myTail _ = []

-- Q7.2
myGCDPatternMatching :: Integral t => t -> t -> t
myGCDPatternMatching a 0 = a
myGCDPatternMatching a b = myGCDPatternMatching b (a `mod` b)