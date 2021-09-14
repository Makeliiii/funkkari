import qualified Data.Map as Map

newtype Box a = Box a deriving Show

-- listing 18.1
wrap :: a -> Box a
wrap = Box

unwrap :: Box a -> a
unwrap (Box x) = x

-- listing 18.2
data Triple a = Triple a a a deriving Show

-- listing 18.3
type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

-- listing 18.4
type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

-- listing 18.5
type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

-- listing 18.6
first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

-- listing 18.7
toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

-- listing 18.8
transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

-- listing 18.9
data List a = Empty | Cons a (List a) deriving Show

-- listing 18.10
builtinEx1 :: [Int]
builtinEx1 = 1:2:3:[]

ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

builtinEx2 :: [Char]
builtinEx2 = 'c':'a':'t':[]

ourListEx2 :: List Char
ourListEx2 = Cons 'c' (Cons 'a' (Cons 't' Empty))

-- listing 18.11
ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap func (Cons a rest) = Cons (func a) (ourMap func rest)

-- listing 18.13
itemCount1 :: (String, Int)
itemCount1 = ("Erasers", 25)

itemCount2 :: (String, Int)
itemCount2 = ("Pencils", 25)

itemCount3 :: (String, Int)
itemCount3 = ("Pens", 13)

-- listing 18.14
itemInventory :: [(String, Int)]
itemInventory = [itemCount1,itemCount2,itemCount3]

-- listing 18.15
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)

-- listing 18.16
organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

-- listing 18.17
ids :: [Int]
ids = [2,7,13,14,21,24]

-- listing 18.19
organPairs :: [(Int, Organ)]
organPairs = zip ids organs

-- listing 18.20
organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- Q18.1
tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box a) = Box (f a)

-- Q18.2
getCount :: Organ -> [Organ] -> Int
getCount organ xs = helper xs 0
    where helper (x:xs) count = if x == organ
                                then helper xs (count + 1)
                                else helper xs count
          helper [] count = count

hearts :: Int
hearts = getCount Heart organs

brains :: Int
brains = getCount Brain organs

kidneys :: Int
kidneys = getCount Kidney organs

spleens :: Int
spleens = getCount Spleen organs

organCounts :: [Int]
organCounts = [hearts, brains, kidneys, spleens]

organss :: [Organ]
organss = [Heart, Brain, Kidney, Spleen]

organInventoryPairs :: [(Organ, Int)]
organInventoryPairs = zip organss organCounts

organInventory :: Map.Map Organ Int
organInventory = Map.fromList organInventoryPairs

-- i could've actually mapped the organ counts
-- well whatever this works too