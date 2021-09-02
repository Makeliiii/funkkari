inc :: Num a => a -> a
inc x = x + 1

double :: Num a => a -> a
double x = x * 2

square :: Int -> Int 
square x = x^2

asdFunc :: Integer -> Integer
asdFunc x = if even x then x - 2 else 3 * x + 1