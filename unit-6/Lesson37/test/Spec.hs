import Test.QuickCheck
import Primes
import Data.Maybe

prop_validPrimesOnly :: Int -> Bool
prop_validPrimesOnly val = if val < 2 || val >= length primes
                           then isNothing result
                           else isJust result
    where
        result = isPrime val

prop_primesArePrime :: Int -> Bool
prop_primesArePrime val = result /= Just True || null divisors
    where
        result = isPrime val
        divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_nonPrimesAreComposite :: Int -> Bool
prop_nonPrimesAreComposite val = result /= Just False || not (null divisors)
    where
        result = isPrime val
        divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_factorsMakeOriginal :: Int -> Bool
prop_factorsMakeOriginal val = isNothing result || (product (fromJust result) == val)
    where
        result = primeFactors val

prop_allFactorsPrime :: Int -> Bool
prop_allFactorsPrime val = isNothing result || all (== Just True) resultsPrime
    where
        result = primeFactors val
        resultsPrime = map isPrime (fromJust result)

main :: IO ()
main = do
    quickCheck prop_validPrimesOnly
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_primesArePrime
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_nonPrimesAreComposite
    quickCheck prop_factorsMakeOriginal
    quickCheck prop_allFactorsPrime
