-- listing 30.12
askForName :: IO ()
askForName = putStrLn "What is your name?"

-- listing 30.13
nameStatement :: String -> String
nameStatement name = "Hello " ++ name ++ "!"

-- quick chekc 30.4
asd :: Num a => a -> IO a
asd = return . (+2)

-- listing 30.14
main :: IO ()
main = askForName >> getLine >>= putStrLn . nameStatement

