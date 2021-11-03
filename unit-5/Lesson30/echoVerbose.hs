-- listing 30.11
echoVerbose :: IO ()
echoVerbose = putStrLn "Enter a String and we'll echo it!" >>
              getLine >>= putStrLn

main :: IO ()
main = echoVerbose