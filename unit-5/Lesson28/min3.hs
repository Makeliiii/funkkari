-- listing 28.7
minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree x y z = min x (min y z)

-- listing 28.8
readInt :: IO Int 
readInt = read <$> getLine

-- listing 28.9
minOfInts :: IO Int 
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

-- listing 28.10
main :: IO ()
main = do
    putStrLn "Enter three numbers"
    minInt <- minOfInts
    putStrLn (show minInt ++ " is the smallest")