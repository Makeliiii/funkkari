import Data.Char

-- 9.2
addAnA :: [[Char]] -> [[Char]]
addAnA [] = []
addAnA (x:xs) = ("a " ++ x):addAnA xs

squareAll :: Num a => [a] -> [a]
squareAll [] = []
squareAll (x:xs) = (x^2):squareAll xs

-- listing 9.2
myMap :: (t -> a) -> [t] -> [a]
myMap f [] = []
myMap f (x:xs) = f x:myMap f xs

-- listing 9.3
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter test [] = []
myFilter test (x:xs) = if test x
                       then x:myFilter test xs
                       else myFilter test xs

-- quick check 9.1
myRemove :: (a -> Bool) -> [a] -> [a]
myRemove test [] = []
myRemove test (x:xs) = if test x
                       then myRemove test xs
                       else x:myRemove test xs

-- quick check 9.2
myProduct :: [Int] -> Int
myProduct = foldl (*) 1

-- asd
concatAll :: Foldable t => t [Char] -> [Char]
concatAll = foldl (++) ""

sumOfSquares :: Num b => [b] -> b
sumOfSquares xs = foldl (+) 0 (map (^2) xs)

-- listing 9.4
rcons :: [a] -> a -> [a]
rcons x y = y:x

myReverse :: Foldable t => t a -> [a]
myReverse xs = foldl rcons [] xs

-- listing 9.5
myFoldl :: (t -> a -> t) -> t -> [a] -> t
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
    where newInit = f init x

-- Q9.1
myElem :: Eq a => a -> [a] -> Bool
myElem s [] = False
myElem s xs = filterLength == 1
    where filtering = filter (== s) xs
          filterLength = length filtering

-- Q9.2
isPalindrome :: [Char] -> Bool
isPalindrome "" = False
isPalindrome xs = filteredList == reverseList
    where lowercaseList = map toLower xs
          filteredList = filter (/= ' ') lowercaseList
          reverseList = reverse filteredList

-- Q9.3
harmonic :: (Fractional b, Enum b) => b -> b
harmonic n = foldl (+) 0 (map (1 /) [1 .. n])