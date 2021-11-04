-- listing 31.1
askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >> getLine >>= putStrLn . nameStatement

-- consider this
largest :: Ord a => (a,a) -> a
largest (x,y) 
    | x > y = x
    | x < y = y
    | otherwise = x

maxPairM :: (Monad m, Ord a) => m (a,a) -> m a
maxPairM m = largest <$> m

juttu :: IO (Int,Int)
juttu = return (2,7)

-- listing 31.2
helloNameDo :: IO ()
helloNameDo = do
    askForName
    name <- getLine 
    putStrLn $ nameStatement name

-- quick check 31.1
echo :: IO ()
echo = do
    pask <- getLine
    print pask
