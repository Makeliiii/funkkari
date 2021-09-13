import Data.List
import Data.Semigroup

-- listing 17.1
myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . map testFunc

-- quick check 17.1
myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . map testFunc

-- listing 17.2
instance Semigroup Integer where
    (<>) x y = x + y

-- listing 17.3
data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown | Identity deriving (Show, Eq)

-- listing 17.4 and 17.5
instance Semigroup Color where
    (<>) Red Blue    = Purple
    (<>) Blue Red    = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red  = Orange
    (<>) Red Yellow  = Orange
    (<>) a b | a == b = a
             | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
             | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
             | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
             | otherwise = Brown

-- listing 17.8
type Events = [String]
type Probs  = [Double]

-- listing 17.9
data PTable = PTable Events Probs

--listing 17.10
createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
    where totalProbs = sum probs
          normalizedProbs = map (/totalProbs) probs

-- listing 17.11
showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

-- listing 17.12
instance Show PTable where
    show (PTable events probs) = mconcat pairs
        where pairs = zipWith showPair events probs

-- listing 17.13
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where nToAdd = length l2
          repeatedL1 = map (take nToAdd . repeat) l1
          newL1 = mconcat repeatedL1
          cycledL2 = cycle l2

-- listing 17.14
combineEvents :: Events -> Events -> Events
combineEvents = cartCombine combiner
    where combiner = \x y -> mconcat [x, "-", y]

combineProbs :: Probs -> Probs -> Probs
combineProbs = cartCombine (*)

-- listing 17.15
instance Semigroup PTable where
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable [] []) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
        where newEvents = combineEvents e1 e2
              newProbs = combineProbs p1 p2

-- listing 17.16
instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)

-- listing 17.17
coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]

-- Q17.1
instance Monoid Color where
    mempty = Identity
    mappend = (<>)

-- Q17.2
{- newtype EventsType = EventsType [String]
newtype ProbsType  = Probs [Int]

instance Semigroup EventsType where
    (<>) e1 (EventsType []) = e1
    (<>) (EventsType []) e2 = e2
    (<>) (EventsType e1) (EventsType e2) = combineEvents e1 e2 -}
-- I don't fucking understand how this is done wtf is this