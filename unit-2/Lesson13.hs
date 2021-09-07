-- listing 13.2
addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2

-- listing 13.3
class Describable a where
    describe :: a -> String

-- listing 13.8 and 13.9
data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

-- Q13.1
-- Word is an unsigned integral type, with the same size as Int

-- Q13.2
-- I don't think there's any difference lul (WRONG)
-- Bounded type doesn't have a true successor, so succ throws an error.
-- The inc function just rotates you back to the beginning.

-- Q13.3
-- cycleSucc :: (Bounded a, Enum a, Num a) => a -> a
-- cycleSucc n = n + 1
-- AAAAAAAnd that was wrong as well nice
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound 
              then minBound
              else succ n