-- quick check 6.1 -- True or False: You can compile and run a program with the variable backwardsInfinity = reverse [1..].
-- True
backwardsInfinity :: [Integer]
backwardsInfinity = reverse [1 ..]

-- listing 6.2
isPalindrome :: [Char] -> Bool
isPalindrome str = str == reverse str

-- listing 6.3
respond :: Foldable t => t Char -> [Char]
respond phrase = if '!' `elem` phrase
                 then "wow!"
                 else "uh.. okay"

-- listing 6.4
takeLast :: Int -> [a] -> [a]
takeLast n aList = reverse (take n (reverse aList))

-- listing 6.5
ones :: Num a => Int -> [a]
ones n = take n (cycle [1])

-- listing 6.6
assignToGroups :: (Num a, Enum a) => a -> [b] -> [(a, b)]
assignToGroups n aList = zip groups aList
    where groups = cycle [1..n]

-- Q6.1
customRepeat :: [a] -> [a]
customRepeat = cycle

-- Q6.2
subseq :: Int -> Int -> [a] -> [a]
subseq start end list = drop start (take end list)

-- Q6.3
inFirstHalf :: Eq a => a -> [a] -> Bool
inFirstHalf ele list = ele `elem` split
    where split = take (length list `div` 2) list