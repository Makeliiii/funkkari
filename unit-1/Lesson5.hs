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