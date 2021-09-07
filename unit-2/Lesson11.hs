-- listing 11.5
half :: Int -> Double 
half x = fromIntegral x / 2

-- quick check 11.1
halve :: Integer -> Integer 
halve x = div x 2

-- quick check 11.2
printDouble :: Integer -> String 
printDouble x = show (x * 2)

-- listing 11.6
anotherNumber :: Int 
anotherNumber = read "6"

makeAddress :: Integer -> String -> String -> (Integer, String, String)
makeAddress number name town = (number, name, town)

makeAddressLambda :: Integer -> String -> String -> (Integer, String, String)
makeAddressLambda = (\number ->
                     (\name ->
                      (\town -> (number, name, town))))

-- listing 11.7
ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n
             then f n
             else n

-- listing 11.8
simpleInt :: Int -> Int
simpleInt n = n

simpleChar :: Char -> Char
simpleChar c = c

-- listing 11.9
simple :: a -> a
simple n = n

-- listing 11.10
makeTriple :: a -> b -> c -> (a,b,c)
makeTriple x y z = (x,y,z)

-- Q11.1
-- filter :: [a] -> (b -> Bool) -> [a]
-- perhaps :thonking:

-- Q11.2
myTail :: [a] -> [a]
myTail [] = []
myTail (x:xs) = xs

-- myHead :: [a] -> 
-- myHead [] = []
-- myHead (x:xs) = x
-- no sane solution for this

-- Q11.3
-- foldl :: (a -> a -> a) -> a -> [a] -> a
-- correct answer apparently:
-- foldl :: (a -> b -> a) -> a -> [b] -> a