module Main where
import qualified Palindrome

main :: IO ()
main = print "Enter a word and I'll let you know if it's a palindrome!" >> getLine >>= (\x -> if Palindrome.isPalindrome x then print "it is!" else print "it's not!") -- probably much more readable in do notation lul

    