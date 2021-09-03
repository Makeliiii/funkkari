import Data.List

isEven :: Integral p => (p -> p) -> p -> p
isEven func x = if even x
    then func x
    else x

names :: [([Char], [Char])]
names = [("Ian", "Curtis"), ("Bernard", "Summer"), ("Peter", "Hook"), ("Stephen", "Morris"), ("Matti", "Summer")]

sorted :: [([Char], [Char])]
sorted = sort names

compareLastNames :: (Ord a1, Ord a2) => (a2, a1) -> (a2, a1) -> Ordering
compareLastNames name1 name2 = if lastName1 > lastName2
                               then GT 
                               else if lastName1 < lastName2
                                   then LT 
                                   else if lastName1 == lastName2
                                       then if firstName1 > firstName2
                                           then GT 
                                           else if firstName1 < firstName2
                                               then LT 
                                               else GT 
                                       else GT 
    where lastName1 = snd name1
          lastName2 = snd name2
          firstName1 = fst name1
          firstName2 = fst name2

-- listing 4.6
addressLetter :: ([Char], [Char]) -> [Char] -> [Char]
addressLetter name location = nameText ++ " - " ++ location
    where nameText = fst name ++ " " ++ snd name

-- listing 4.7
sfOffice :: ([Char], [Char]) -> [Char]
sfOffice name = if lastName < "L"
                then nameText
                     ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText
                     ++ " - PO Box 1010 - San Francisco, CA, 94109"
    where lastName = snd name
          nameText = fst name ++ " " ++ lastName

nyOffice :: ([Char], [Char]) -> [Char]
nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = fst name ++ " " ++ snd name

renoOffice :: (a, [Char]) -> [Char]
renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
    where nameText = snd name

-- listing 4.8
getLocationFunction :: [Char] -> ([Char], [Char]) -> [Char]
getLocationFunction location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "wa" -> waOffice
    _ -> (\name -> fst name ++ " " ++ snd name)

-- listing 4.9
addressLetterBetter :: ([Char], [Char]) -> [Char] -> [Char]
addressLetterBetter name location = locationFunction name
    where locationFunction = getLocationFunction location

-- lesson 4 Q4.1
compareLastNamesNew :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
compareLastNamesNew name1 name2 = if comparisonResult == EQ 
                               then compare firstName1 firstName2
                               else comparisonResult
    where lastName1 = snd name1
          lastName2 = snd name2
          firstName1 = fst name1
          firstName2 = fst name2
          comparisonResult = compare lastName1 lastName2

-- lesson 4 Q4.1
waOffice :: ([Char], [Char]) -> [Char]
waOffice name = nameText ++ " Esq - PO Box 666 - Washington, DC 66666"
    where nameText = fst name ++ " " ++ snd name

-- listing 5.1
inc :: Num a => a -> a
inc n = n + 1

double :: Num a => a -> a
double n = n * 2

square :: Num a => a -> a
square n = n^2

ifEven :: Integral p => (p -> p) -> p -> p
ifEven func n = if even n
           then func n
           else n

genIfEven :: Integral p => (p -> p) -> p -> p
genIfEven f = (\x -> ifEven f x)

ifEvenIncasd :: Integer -> Integer
ifEvenIncasd = genIfEven inc

genIfXEven :: Integral p => p -> (p -> p) -> p
genIfXEven x = (\f -> ifEven f x)

-- listing 5.2
getRequestUrl :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
getRequestUrl host apiKey resource id = host ++
                                        "/" ++
                                        resource ++
                                        "/" ++
                                        id ++
                                        "?token=" ++
                                        apiKey
                    
genHostRequestBuilder :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
genHostRequestBuilder host = (\apiKey resource id ->
                               getRequestUrl host apiKey resource id)

-- listing 5.3
exampleUrlBuilder :: [Char] -> [Char] -> [Char] -> [Char]
exampleUrlBuilder = genHostRequestBuilder "https://example.com"

-- listing 5.4
genApiRequestBuilder :: (t1 -> t2 -> t3 -> t4) -> t1 -> t2 -> t3 -> t4
genApiRequestBuilder hostBuilder apiKey = (\resource id ->
                                            hostBuilder apiKey resource id)

-- listing 5.5
myExampleUrlBuilder :: [Char] -> [Char] -> [Char]
myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"

-- quick check 5.2
genApiRequestBuilderWithResource :: (t1 -> t2 -> t3 -> t4) -> t1 -> t2 -> t3 -> t4
genApiRequestBuilderWithResource hostBuilder apiKey resource = (\id ->
                                                                 hostBuilder apiKey resource id)

myExampleUrlBuilderAsd :: [Char] -> [Char]
myExampleUrlBuilderAsd = genApiRequestBuilderWithResource exampleUrlBuilder "1337hAsk3ll" "book" 

-- listing 5.6
exampleUrlBuilderTwo :: [Char] -> [Char] -> [Char] -> [Char]
exampleUrlBuilderTwo = getRequestUrl "https://example.com"

myExampleUrlBuilderTwo :: [Char] -> [Char] -> [Char]
myExampleUrlBuilderTwo = exampleUrlBuilderTwo "1337hAsk3ll"

-- quick check 5.3
exampleUrlBuilderWithResource :: [Char] -> [Char]
exampleUrlBuilderWithResource = myExampleUrlBuilderTwo "book"

-- quick check 5.4
subtract2 :: Integer -> Integer
subtract2 = flip (-) 2

-- Q5.1
ifEvenInc :: Integer -> Integer
ifEvenInc = ifEven inc

ifEvenDouble :: Integer -> Integer
ifEvenDouble = ifEven double

ifEvenSquare :: Integer -> Integer 
ifEvenSquare = ifEven square

-- Q5.2
binaryPartialApplication :: (t1 -> t2 -> t3) -> t1 -> t2 -> t3
binaryPartialApplication func x = (\y -> func x y)

partialApplication :: [Char] -> [Char]
partialApplication = binaryPartialApplication (\x y -> x ++ y) "hello"