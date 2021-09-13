-- listing 16.2
-- data AuthorName = AuthorName String String
-- data Book = Author String String Int

-- listing 16.3
{- data Book = Book {
    author :: AuthorName,
    isbn   :: String,
    title  :: String,
    year   :: Int,
    price  :: Double
} -}

-- quick check 16.1
data AuthorName = AuthorName {
    firstName :: String,
    lastName  :: String
}

-- listing 16.10
data Creator = AuthorCreator Author | ArtistCreator Artist

-- listing 16.11
data Author = Author Name

-- listing 16.12
data Artist = Person Name | Band String

type FirstName  = String 
type LastName   = String
type MiddleName = String

-- listing 16.13
data Name = 
      Name FirstName LastName
    | NameWithMiddle FirstName MiddleName LastName
    | TwoInitialsWithLast Char Char LastName
    | FirstNameWithTwoInits FirstName Char Char

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

-- listing 16.16
data Book = Book {
    author    :: Creator,
    isbn      :: String,
    bookTitle :: String,
    bookYear  :: Int,
    bookPrice :: Double
}

-- listing 16.17
data VinylRecord = VinylRecord {
    artist      :: Creator,
    recordTitle :: String,
    recordYear  :: Int,
    recordPrice :: Double
}

-- listing 16.18
-- data StoreItem = BookItem Book | RecordItem VinylRecord

-- listing 16.19
data CollectibleToy = CollectibleToy {
    name        :: String,
    description :: String,
    toyPrice    :: Double
}

-- listing 16.20
data StoreItem = BookItem Book | RecordItem VinylRecord | ToyItem CollectibleToy | PamphletItem Pamphlet

-- listing 16.21
price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem pamphlet) = 0.0

-- quick check 16.22
{- madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = "unknown" -}

-- Q16.1
data Pamphlet = Pamphlet {
    pamphletTitle       :: String,
    pamphletDescription :: String,
    contact             :: String
}

-- Q16.2
data Shape = CircleShape Circle | SquareShape Square | RectangleShape Rectangle

data Circle = Circle {
    r :: Double
}

data Square = Square {
    side :: Double
}

data Rectangle = Rectangle {
    rectangleLength :: Double,
    rectangleWidth :: Double
}

perimeter :: Shape -> Double
perimeter (CircleShape circle) = 2 * pi * r circle
perimeter (SquareShape square) = 4 * side square
perimeter (RectangleShape rectangle) = 2 * (rectangleLength rectangle + rectangleWidth rectangle)

area :: Shape -> Double
area (CircleShape circle) = pi * r circle^2
area (SquareShape square) = 2 * side square
area (RectangleShape rectangle) = rectangleLength rectangle * rectangleWidth rectangle