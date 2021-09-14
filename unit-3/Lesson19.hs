import qualified Data.Map as Map
import Data.List

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- listing 19.2
possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

-- listing 19.3
getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map lookup ids
    where lookup id = Map.lookup id catalog

-- listing 19.4
availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

-- listing 19.5
countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ xs = length (filter (== Just organ) xs)

-- listing 19.6
isSomething :: Maybe Organ -> Bool
isSomething Nothing  = False
isSomething (Just _) = True

-- listing 19.7
justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

-- listing 19.8
showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing      = ""

-- listing 19.9
organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = intercalate ", " organList

-- quick check 19.2
numOrZero :: Maybe Int -> Int
numOrZero (Just x) = x
numOrZero Nothing  = 0

-- listing 19.10
data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
    show (Vat organ)    = show organ ++ " in a vat"
    show (Cooler organ) = show organ ++ " in a cooler"
    show (Bag organ)    = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a)    = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a)    = (Kitchen, Bag a)

-- listing 19.11
process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location, Container) -> String
report (location, container) = show container ++ " in the " ++ show location

-- listing 19.12
-- ideal definition
-- won't compile
-- processRequest :: Int -> Map.Map Int Organ -> String
-- processRequest id catalog = report (process organ)
--     where organ = Map.lookup id catalog

-- listing 19.13
processAndReport :: Maybe Organ -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "error, id not found"

-- listing 19.14
processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
    where organ = Map.lookup id catalog

-- quick check 19.3
reportMaybe :: Maybe (Location, Container) -> String
reportMaybe (Just (location, container)) = show container ++ " in the " ++ show location
reportMaybe Nothing = "Container not found"

-- Q19.1
emptyDrawers :: [Int] -> Map.Map Int Organ -> Int
emptyDrawers ids catalog = length (filter (== Nothing) nothings)
    where nothings = getDrawerContents ids catalog

-- Q19.2
maybeMap :: (Maybe a -> Maybe b) -> [Maybe a] -> [Maybe b]
maybeMap func (x:xs) = func x : maybeMap func xs
maybeMap _ []        = []

-- aaaand this is what the book actually wanted for Q19.2
mMap :: (a -> b) -> Maybe a -> Maybe b
mMap func (Just a) = Just (func a)
mMap func Nothing  = Nothing