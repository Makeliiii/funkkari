-- listing 22.7
{- main :: IO ()
main = do
    userInput <- getContents 
    mapM_ print userInput -}

-- quick check 22.3
perhapsMain :: IO ()
perhapsMain = do
    userInput <- getContents 
    let reversedInput = reverse userInput
    putStrLn reversedInput

-- listing 22.8
sampleData :: [Char]
sampleData = ['6', '2', '\n', '2', '1', '\n']

-- listing 22.9 don't have this func zzz
-- myLines = splitOn "\n"

-- listing 22.10
toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
    userInput <- getContents 
    let numbers = toInts userInput
    print (sum numbers)

-- quick check 22.4
maybeMain :: IO ()
maybeMain = do
    userInput <- getContents 
    let numbers = toInts userInput
    let squares = map (^2) numbers
    print (sum squares)