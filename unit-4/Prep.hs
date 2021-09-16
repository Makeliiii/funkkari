-- listing 2
mystery1 :: Int -> Int -> Int
mystery1 x y = (x + y + z)^2
    where z = 3

mystery2 :: Int -> Int -> IO Int
mystery2 x y = do
    putStrLn "Enter a number"
    zInput <- getLine
    let z = read zInput
    return ((x + y + z)^2)
