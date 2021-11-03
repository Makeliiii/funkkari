readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print (n*2)

yup :: IO ()
yup = readInt >>= printDouble