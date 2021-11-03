-- quick check 29.1
combined :: Maybe [Char]
combined = (++) <$> Just "a" <*> Just "atami"

-- quick check 29.2
str :: String
str = "Hello World"

ioStr :: IO String
ioStr = pure str

-- listing 29.1
data Blah a b = Blah a b

-- listing 29.2
newtype Box a = Box a

-- listing 29.3
data ResourceConstrained a = NoResource | Okay a

-- listing 29.4
doorPrize :: [Int]
doorPrize = [1000,2000,3000]

-- listing 29.5
boxPrize :: [Int]
boxPrize = [50,20000]

-- listing 29.7
primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
    where
        twoThroughN = [2 .. n]
        composite = (*) <$> twoThroughN <*> twoThroughN
        isNotComposite = not . (`elem` composite)

-- listing 29.8
testNames :: [String]
testNames = ["John Smith",
             "Robert '); DROP TABLE Students;--",
             "Christina NULL",
             "Randall Munroe"]

-- listing 29.9
testIds :: [Int]
testIds = [1337, 0123, 999999]

-- listing 29.10
testScores :: [Int]
testScores = [0, 10000, -999999]

data User = User {
    name :: String,
    gamerId :: Int,
    score :: Int
} deriving Show

-- listing 29.11
testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores

-- Q29.1
allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap = fmap

-- Q29.2
exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6