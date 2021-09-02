-- lesson 2 think
inc :: Num a => a -> a
inc x = x + 1

double :: Num a => a -> a
double x = x * 2

square :: Int -> Int 
square x = x^2

asdFunc :: Integer -> Integer
asdFunc x = if even x then x - 2 else 3 * x + 1

-- lesson 3
-- q3.1 write all functions as lambda functions
sumSquareOrSquareSum :: Int -> Int -> Int 
sumSquareOrSquareSum x y = (\sumSquare squareSum ->
    if sumSquare > squareSum
        then sumSquare
        else squareSum) (x^2 + y^2) ((x+y)^2)

overwrite x = (\x -> (\x -> (\x -> x)4 )3 )2

counter x = (\x -> (\x -> x + 1)x + 1)x