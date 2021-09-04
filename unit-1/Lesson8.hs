-- consider this
-- this took way longer than it should've
myDrop :: Int -> [Int] -> [Int]
myDrop n list = if n > length list
                then error "n larger than list length"
                else edit n list []
    where listLength = length list
          edit n currList list = if n == length currList
                                 then list
                                 else edit (n + 1) currList (list ++ [currList !! n])

-- listing 8.1
myLength :: Num p => [a] -> p
myLength [] = 0
myLength xs = 1 + myLength(tail xs)

-- listing 8.2
myTake :: (Eq a1, Num a1) => a1 -> [a2] -> [a2]
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x:rest
    where rest = myTake (n - 1) xs

-- listing 8.3
myCycle :: [a] -> [a]
myCycle (first:rest) = first:myCycle (rest ++ [first])
myCycle [] = []

-- ackerman function
ackerman :: (Num a, Num t, Eq a, Eq t) => a -> t -> t
ackerman 0 n = n + 1
ackerman m 0 = ackerman (m - 1) 1
ackerman m n = ackerman (m - 1) (ackerman m (n - 1))

-- collatz function
collatz :: (Num p, Integral a) => a -> p
collatz 1 = 1
collatz n = if even n
            then 1+ collatz (n `div` 2)
            else 1+ collatz (n * 3 + 1)

-- Q8.1
myReverse :: [a] -> [a]
myReverse xs = helper n xs []
    where n = length xs
          helper n xs list = if n == 0
                             then list
                             else helper (n - 1) xs (list ++ [xs !! (n - 1)])

-- Q8.2 didn't actually figure this out sadge
fastFib :: (Eq t1, Num t1, Num t2) => t2 -> t2 -> t1 -> t2
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y c = fastFib (x + y) x (c - 1)
fib :: (Eq t1, Num t1, Num t2) => t1 -> t2
fib n = fastFib 1 1 n
