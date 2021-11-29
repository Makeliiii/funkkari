module Primes where

primes :: [Int]
primes = sieve [2 .. 10000]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve noFactors
    where
        noFactors = filter ((/= 0) . (`mod` x)) xs

isPrime :: Int -> Maybe Bool 
isPrime n 
    | n < 2 = Nothing 
    | n >= length primes = Nothing
    | otherwise = Just (n `elem` primes)

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors n [] = []
unsafePrimeFactors n (x:xs) = if n `mod` x == 0
                              then x : unsafePrimeFactors (n `div` x) (x:primes)
                              else unsafePrimeFactors n primes

primeFactors :: Int -> Maybe [Int]
primeFactors n
    | n < 2 = Nothing
    | n >= length primes = Nothing 
    | otherwise = Just (unsafePrimeFactors n primesLessThanN)
        where
            primesLessThanN = filter (<= n) primes