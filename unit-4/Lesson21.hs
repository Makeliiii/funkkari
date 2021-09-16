import qualified Data.Map as Map

-- listing 21.1
{- helloPerson :: String -> String 
helloPerson name = "Hello " ++ name ++ "!"

main :: IO ()
main = do
    putStrLn "Hello! What's your name?"
    name <- getLine 
    let statement = helloPerson name
    putStrLn statement -}

-- listing 21.2
{- import System.Random

minDie :: Int
minDie = 1

maxDie :: Int 
maxDie = 6

main :: IO ()
main = do
    dieRoll <- randomRIO (minDie, maxDie)
    putStrLn (show dieRoll) -}

-- listing 21.4
areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2)^2

-- listing 21.5
type Pizza = (Double, Double)

-- listing 21.6
costPerCM :: Pizza -> Double
costPerCM (size, cost) = cost / areaGivenDiameter size

-- listing 21.7
comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                      then p1
                      else p2
    where costP1 = costPerCM p1
          costP2 = costPerCM p2

-- listing 21.8
describePizza :: Pizza -> String
describePizza (size, cost) = "The " ++ show size ++ " pizza " ++
                             "is cheaper at " ++
                             show costSqCm ++
                             " per square centimeter."
    where costSqCm = costPerCM (size, cost)

-- listing 21.9
{- main :: IO ()
main = do
    putStrLn "What is the size of pizza 1"
    size1 <- getLine 
    putStrLn "What is the cost of pizza 1"
    cost1 <- getLine 
    putStrLn "What is the size of pizza 2"
    size2 <- getLine 
    putStrLn "What is the cost of pizza 2"
    cost2 <- getLine 
    let pizza1 = (read size1, read cost1)
    let pizza2 = (read size2, read cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn (describePizza betterPizza) -}

-- listing 21.10
costData :: Map.Map Int Double
costData = Map.fromList [(1, 18.0), (2, 16.0)]

-- listing 22.11
sizeData :: Map.Map Int Double 
sizeData = Map.fromList [(1, 20.0), (2, 15.0)]

-- listing 21.12
maybeMain :: Maybe String
maybeMain = do
    size1 <- Map.lookup 1 sizeData
    cost1 <- Map.lookup 1 costData
    size2 <- Map.lookup 2 sizeData
    cost2 <- Map.lookup 2 costData
    let pizza1 = (size1, cost1)
    let pizza2 = (size2, cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)

-- Q21.1
helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

nameData :: Map.Map Int String
nameData = Map.fromList [(1, "James")]

perhapsMain :: Maybe String
perhapsMain = do
    name <- Map.lookup 1 nameData
    let statement = helloPerson name
    return statement

-- Q22.1
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
    putStrLn "Enter a number :)"
    n <- getLine 
    let fibo = fib (read n)
    putStrLn ("The fib number is " ++ show fibo)