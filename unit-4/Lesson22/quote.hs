quotes :: [String]
quotes = [
    "Your mom hahaaaa",
    "Libertine Dissolves",
    "Bedrooms",
    "Daedalus",
    "Exit Denied"
    ]

lookupQuote :: [String] -> [String]
lookupQuote [] = []
lookupQuote ("n":xs) = []
lookupQuote (x:xs) = quote : lookupQuote xs
    where quote = quotes !! (read x - 1)

main :: IO ()
main = do
    input <- getContents 
    mapM_ putStrLn (lookupQuote (lines input))

