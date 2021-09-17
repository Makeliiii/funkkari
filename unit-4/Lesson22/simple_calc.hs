-- this obviously doesn't work
{- import Data.List.Split

getOperation :: String -> Char
getOperation xs = xs !! half
    where half = length xs `div` 2

getOperands :: Char -> String -> [Int]
getOperands n = map read . splitOn [n]

main :: IO ()
main = do
    input <- getContents
    let operation = getOperation input
    let operands = getOperands operation input

    if operation == '+'
    then print (sum operands)
    else if operation == '*'
         then print (product operands)
    else
        print "Nå I don't think så" -}

-- ok so apparently i was supposed to do lines huh
-- fuck this
calc :: [String] -> Int
calc (x:"+":y:rest) = read x + read y
calc (x:"*":y:rest) = read x * read y

main :: IO ()
main = do
    userInput <- getContents 
    let values = lines userInput
    print (calc values)