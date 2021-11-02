-- listing 28.11
data User = User {
    name :: String,
    gamerId :: Int,
    score :: Int
} deriving Show

-- listing 28.12
serverUsername :: Maybe String 
serverUsername = Just "Sue"

serverGamerId :: Maybe Int 
serverGamerId = Just 1337

serverScore :: Maybe Int 
serverScore = Just 9001

-- listing 28.13
readInt :: IO Int 
readInt = read <$> getLine

main :: IO ()
main = do
    putStrLn "Enter a username, gamerId and score"
    user <- User <$> getLine <*> readInt <*> readInt
    print user