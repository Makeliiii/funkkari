import Lib (isPalindrome)
import Test.QuickCheck

assert :: Bool -> String -> String -> IO ()
assert test pass fail = if test then putStrLn pass else putStrLn fail

main :: IO ()
main = do
    putStrLn "Running tests..."
    assert (isPalindrome "racecar") "passed 'racecar'" "FAIL: 'racecar'"
    assert (isPalindrome "racecar!") "passed 'racecar!'" "FAIL: 'racecar!'"
    assert (not $ isPalindrome "cat") "passed 'cat'" "FAIL: 'cat'"
    assert (isPalindrome "racecar.") "passed 'racecar.'" "FAIL: 'racecar.'"
    putStrLn "done!"
