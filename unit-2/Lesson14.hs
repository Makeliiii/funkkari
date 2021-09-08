-- listing 14.1 and 14.2
-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show)

-- listing 14.3
{- instance Show SixSidedDie where
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four"
    show S5 = "five"
    show S6 = "six" -}

-- quick check 14.1
-- roman numerals
instance Show SixSidedDie where
    show S1 = "I"
    show S2 = "II"
    show S3 = "III"
    show S4 = "IV"
    show S5 = "V"
    show S6 = "VI"

{- instance Eq SixSidedDie where
    (==) S6 S6 = True
    (==) S5 S5 = True
    (==) S4 S4 = True
    (==) S3 S3 = True
    (==) S2 S2 = True
    (==) S1 S1 = True
    (==) _  _  = False -}

-- Quick check 14.2
-- Use Hoogle to search for the RealFrac type class. What's its minimal complete definition?
-- It seems to be properFraction, which is defined as:
-- properFraction :: Integral b => a -> (b, a)

-- Listing 14.7
{- instance Ord SixSidedDie where
    compare S6 S6 = EQ
    compare S6 _  = GT
    compare _  S6 = LT
    compare S5 S5 = EQ
    compare S5 _  = GT
    compare _  S5 = LT
    compare S4 S4 = EQ
    compare S4 _  = GT
    compare _  S4 = LT -}

-- listing 14.8
data Test1 = AA | ZZ deriving (Eq, Ord)
data Test2 = ZZZ | AAA deriving (Eq, Ord)

-- quick check 14.4 and a part of listing 14.9
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Ord, Enum)

-- listing 14.9
{- instance Enum SixSidedDie where
    toEnum 0 = S1
    toEnum 1 = S2
    toEnum 2 = S3
    toEnum 3 = S4
    toEnum 4 = S5
    toEnum 5 = S6
    toEnum _ = error "No such value"

    fromEnum S1 = 0
    fromEnum S2 = 1
    fromEnum S3 = 2
    fromEnum S4 = 3
    fromEnum S5 = 4
    fromEnum S6 = 5 -}

-- ölisting 14.10
-- type Name = (String, String)

{- names :: [Name]
names = [
    ("Emil", "Cioran"),
    ("Eugene", "Thacker"),
    ("Friedrich", "Nietzsche")
    ] -}

-- listing 14.11
{- instance Ord Name where
    compare (f1, l1) (f2, l2) = compare (l1, f1) (l2, f2) -}

-- listing 14.12
data Name = Name (String, String) deriving (Show, Eq)

-- listing 14.13
instance Ord Name where
    compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

names :: [Name]
names = [
    Name ("Emil", "Cioran"),
    Name ("Eugene", "Thacker"),
    Name ("Friedrich", "Nietzsche")
    ]

-- Q14.1
data Number = One | Two | Three deriving (Enum)

instance Eq Number where
    (==) x y = fromEnum x == fromEnum y

instance Ord Number where
    compare x y = compare (fromEnum x) (fromEnum y)

-- Q14.2
data FiveSidedDie = A1 | A2 | A3 | A4 | A5 deriving (Show, Eq, Ord)

class Die a where
    roll :: a -> String

instance Die FiveSidedDie where
    roll die = show die

die :: FiveSidedDie
die = A5