import Control.Monad (guard)
import Data.Char (toUpper)
-- listing 32.2
powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
    value <- [1 .. n]
    let powersOfTwo =  2^value
    let powersOfThree = 3^value
    return (powersOfTwo,powersOfThree)

allEvenOdds :: Int -> [(Int,Int)]
allEvenOdds n = do
    evenV <- [2,4 .. n]
    oddV  <- [1,3 .. n]
    return (evenV,oddV)

-- quick check 32.1
squarePairs :: [(Int,Int)]
squarePairs = do
    val <- [1 .. 10]
    return (val,val^2)

-- quick check 32.2
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = do
    val <- xs
    guard (f val)
    return val

-- listing 32.3
evenSquares :: [Int]
evenSquares = do
    n <- [0 .. 9]
    let nSquared = n^2
    guard (even nSquared)
    return nSquared

-- quick check 32.3
pogchamp :: [String]
pogchamp = ["brown", "blue", "pink", "orange"]

-- less verbose than the book solution btw
noKap :: [String] -> [String]
noKap xs = [toUpper y : ys  | (y:ys) <- xs]

